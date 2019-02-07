;;; make-ide.el --- Calls CMakec to find out include paths and other compiler flags -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Atila Neves

;; Author:  Atila Neves <atila.neves@gmail.com>
;; Version: 0.6
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5") (seq "1.11") (levenshtein "0") (s "1.11.0"))
;; Keywords: languages
;; URL: http://github.com/atilaneves/cmake-ide

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package runs Make and sets variables for IDE-like functionality
;; provided by other packages such as:
;; On the fly syntax checks with flycheck
;; auto-completion using auto-complete-clang or company-clang
;; Jump to definition and refactoring with rtags
;; These other packages must be installed for the functionality to work

;;; Usage:

;; (make-ide-setup)
;;
;; If make-ide-flags-c or make-ide-flags-c++ are set, they will be added to ac-clang-flags.
;; These variables should be set. Particularly, they should contain the system include paths.
;;
;;; Code:

(require 'json)
(require 'find-file)
(require 'levenshtein)
(require 'cl-lib)
(require 'seq)
(require 's)
(require 'dash)

(defsubst mide--string-empty-p (string)
  "Check whether STRING is empty."
  (string= string ""))

(if (not (require 'subr-x nil t))
    (progn
      (message "`subr-x' not found, replacing string-empty-p with mide--string-empty-p")
      (fset 'string-empty-p 'mide--string-empty-p))
  (declare-function string-empty-p "subr-x"))

(declare-function rtags-call-rc "rtags")
(declare-function rtags-executable-find "rtags")
(declare-function irony-cdb-json-add-compile-commands-path "irony")
(declare-function flycheck-clear "flycheck")


(defcustom make-ide-flags-c
  nil
  "The C compiler flags to use.  Should have -I flags for system includes."
  :group 'make-ide
  :type 'string
  :safe #'stringp)

(defcustom make-ide-flags-c++
  nil
  "The C++ compiler flags to use.  Should have -I flags for system includes."
  :group 'make-ide
  :type 'string
  :safe #'stringp
  )

(defcustom make-ide-dir
  nil
  "The build directory to run Make in.  If nil, runs in a temporary directory under `make-ide-build-pool-dir'.  DEPRECATED, use `make-ide-build-dir' instead."
  :group 'make-ide
  :type 'directory
  :safe #'stringp
  )

(defcustom make-ide-build-dir
  nil
  "The build directory to run Make in.  If nil, runs in a temporary directory under `make-ide-build-pool-dir'."
  :group 'make-ide
  :type 'directory
  :safe #'stringp
  )

(defcustom make-ide-build-pool-dir
  temporary-file-directory
  "The parent directory for all automatically created build directories."
  :group 'make-ide
  :type 'directory
  :safe #'stringp
  )

(defcustom make-ide-build-pool-use-persistent-naming
  nil
  "Whether or not to use a persistent naming scheme for all automatically created build directories."
  :group 'make-ide
  :type 'boolean
  :safe #'booleanp)

(defcustom make-ide-project-dir
  nil
  "The project directory."
  :group 'make-ide
  :type 'directory
  :safe #'stringp)

(defcustom make-ide-compile-command
  nil
  "The command to use to compile the project.  Can also include running tests."
  :group 'make-ide
  :safe #'stringp)

(defcustom make-ide-ninja-command
  "ninja"
  "The command used to execute ninja type builds."
  :group 'make-ide
  :safe #'stringp)

(defcustom make-ide-make-command
  "make"
  "The command use to invoke make."
  :group 'make-ide
  :safe #'stringp)

(defcustom make-ide-make-opts
  ""
  "The options passed to make when calling it.  DEPRECATED, use `make-ide-make-args' instead."
  :group 'make-ide
  :safe #'stringp)

(defcustom make-ide-make-args
  nil
  "The options passed to make when calling it."
  :group 'make-ide
  :type '(repeat string))

(defcustom make-ide-header-search-other-file
  t
  "Whether or not to search for a corresponding source file for headers when setting flags for them."
  :group 'make-ide
  :type 'boolean
  :safe #'booleanp)

(defcustom make-ide-header-search-first-including
  t
  "Whether or not to search for the first source file to include a header when setting flags for them."
  :group 'make-ide
  :type 'boolean
  :safe #'booleanp)

(defcustom make-ide-header-no-flags
  nil
  "Whether to apply compiler flags to header files.  In some projects this takes too long."
  :group 'make-ide
  :type 'boolean
  :safe #'booleanp)

(defcustom make-ide-flycheck-cppcheck-strict-standards
  nil
  "Whether or not to be strict when setting cppcheck standards for flycheck.
If 't' or otherwise non-nil, the `flycheck-cppcheck-standards'
variable will only be set to contain standards that exactly match
those from the compile database.  (If there are none, it will not
be modified.)  If 'nil', standards will be gracefully degraded to
the closest possible matches available in cppcheck."
  :group 'make-ide
  :type 'boolean
  :safe #'booleanp)

;;; The buffers to set variables for
(defvar mide--src-buffers nil)
(defvar mide--hdr-buffers nil)

(defcustom make-ide-rdm-executable
  "rdm"
  "Location of rdm executable."
  :group 'rtags
  :type 'file)

(defcustom make-ide-rdm-rc-path
  ""
  "Location of a custom rdm run control file."
  :group 'make-ide
  :type 'string
  :safe #'stringp)

(defcustom make-ide-src-extensions
  '(".c" ".cpp" ".C" ".cxx" ".cc")
  "A list of file extensions that qualify as source files."
  :group 'make-ide
  :type '(repeat string))

(defcustom make-ide-ac-flags-to-filter
  '("-m32" "-m64" "-Werror" "-c" "-fPIC" "-pipe" "-g" "-ggdb" "-march=native")
  "Flags to remove from arguments passed to auto-completion."
  :group 'make-ide
  :type '(repeat string))

(defcustom make-ide-makelists-dir
  nil
  "The directory where the main Makelists.txt is.  DEPRECATED use `make-ide-projet-dir' instead."
  :group 'make-ide
  :type 'file)

(defvar make-ide-try-unique-compiler-flags-for-headers
  nil
  "Whether or not to try all unique compiler flags for header files."
  )

(defun mide--make-hash-table ()
  "Make a hash table with equal for the test function."
  (make-hash-table :test #'equal))

(defvar mide--cache-dir-to-idb
  (mide--make-hash-table)
  "Key: build directory.  Value: IDB for that build directory.")

(defvar mide--cache-dir-to-cdb-hash
  (mide--make-hash-table)
  "Key: build directory.  Value: The hash of the JSON CDB.")

(defvar mide--cache-pkey-to-dir
  (mide--make-hash-table)
  "Key: project key.  Value: build dir.")

;; Build dirs we've already told irony about
(defvar mide--cache-irony-dirs
  (mide--make-hash-table)
  "Used as a set.  Key: build dir.  Value: T or nil.")

(defvar mide--semantic-system-include)

(defconst make-ide-rdm-buffer-name "*rdm*" "The rdm buffer name.")

(defun mide--build-dir-var ()
  "Return the value of `make-ide-build-dir' or `make-ide-dir'."
  (let ((ret (or make-ide-build-dir make-ide-dir)))
    (when ret (file-name-as-directory ret))))

(defun mide--project-dir-var ()
  "Return the value of `make-ide-project-dir' or `make-ide-makelists-dir'."
  (or make-ide-project-dir make-ide-makelists-dir))

(defun mide--mode-hook()
  "Function to add to a major mode hook"
  (add-hook 'find-file-hook #'make-ide-maybe-run-make nil 'local)
  (make-ide-maybe-start-rdm))

;;;###autoload
(defun make-ide-setup ()
  "Set up the Emacs hooks for working with Make projects."
  (add-hook 'c-mode-hook #'mide--mode-hook)
  (add-hook 'c++-mode-hook #'mide--mode-hook)

  ;; When creating a file in Emacs, run Make again to pick it up
  (add-hook 'before-save-hook #'mide--before-save))

(defun mide--before-save ()
  "When creating a file in Emacs, run Make again to pick it up."
  (when (and (mide--is-src-file (buffer-file-name))
             (not (file-readable-p (buffer-file-name))))
    (add-hook 'after-save-hook 'mide--new-file-saved nil 'local)))

(defun mide--new-file-saved ()
  "Run Make to pick up newly created files."
  (make-ide-run-make)
  (remove-hook 'after-save-hook 'mide--new-file-saved 'local))

;;;###autoload
(defun make-ide-maybe-run-make ()
  "Run Make if the compilation database JSON file is not found."
  (interactive)
  (when (mide--locate-project-dir)
    (make-ide-maybe-start-rdm)
    (if (mide--need-to-run-make)
        (make-ide-run-make)
      (progn
        (mide--add-file-to-buffer-list)
        (mide--on-make-finished)))))

(defun mide--add-file-to-buffer-list ()
  "Add buffer to the appropriate list for when Make finishes running."
  (if (mide--is-src-file buffer-file-name)
      (add-to-list 'mide--src-buffers (current-buffer))
    (add-to-list 'mide--hdr-buffers (current-buffer))))

(defun mide--comp-db-file-name ()
  "The name of the compilation database file."
  (when (mide--build-dir)
    (expand-file-name "compile_commands.json" (mide--build-dir))))

(defun mide--need-to-run-make ()
  "If Make needs to be run or not."
  (and (not (get-process "make")) ; don't run if already running
       (not (file-exists-p (mide--comp-db-file-name))))) ; no need if the file exists

;;;###autoload
(defun make-ide-run-make ()
  "Run Make and set compiler flags for auto-completion and flycheck.
This works by calling make in a temporary directory (or `make-ide-build-dir')
 and parsing the JSON file deposited there with the compiler
 flags."
  (interactive)
  (when (buffer-file-name) ; if we call make-ide-run-make from a scatch buffer, do nothing
    (when (file-readable-p (buffer-file-name)) ; new files need not apply
      (save-some-buffers 1)
      (let ((project-dir (mide--locate-project-dir)))
        (if project-dir ; no point if it's not a Make project
            ;; register this buffer to be either a header or source file
            ;; waiting for results
            (progn
              (mide--add-file-to-buffer-list)
              ;; run make only if project dir contains a MakeLists.txt file.
              (if (mide--locate-makelists)
                  (let ((make-dir (mide--build-dir)))
                    (let ((default-directory project-dir))
                      (mide--run-make-impl project-dir make-dir)
                      (mide--register-callback)))
                (mide--message "No Makefile found in project dir, skip make run.")))
          (mide--message "try to run make on a non make project [%s]" default-directory))))))


(defun mide--message (str &rest vars)
  "Output a message with STR and formatted by VARS."
  (message (apply #'format (concat "make-ide [%s]: " str) (cons (current-time-string) vars))))

(defun mide--register-callback ()
  "Register callback for when Make finishes running."
  (mide--register-a-callback
   (lambda (process _event)
     (mide--message "Finished running make")
     (if (= 0 (process-exit-status process)) ; only perform post make operation on success.
         (mide--on-make-finished)
       (mide--message "Make failed, see *make* for details.")))))

(defun mide--register-a-callback (callback)
  "Register CALLBACK to be called when Make finishes running."
  (set-process-sentinel (get-process "make") callback))

(defun mide--on-make-finished ()
  "Set compiler flags for all buffers that requested it."
    (setq mide--src-buffers nil mide--hdr-buffers nil)
    (mide--run-rc))


;;;###autoload
(defun make-ide-load-db ()
  "Load compilation DB and set flags for current buffer."
  (interactive)
  (when (mide--locate-project-dir)
    (mide--message "make-ide-load-db for file %s" (buffer-file-name))
    (make-ide-maybe-start-rdm)
    (let* ((file-name buffer-file-name)
           (buffers (list (current-buffer)))
           (mide--src-buffers (if (mide--is-src-file file-name) buffers nil))
           (mide--hdr-buffers (if (mide--is-src-file file-name) nil buffers)))
      (mide--on-make-finished))))

(defvar mide--rdm-executable nil
  "Rdm executable location path.")

(defun make-ide-rdm-executable ()
  "Return rdm executable location path."
  (cond (mide--rdm-executable mide--rdm-executable)
        ((file-exists-p make-ide-rdm-executable)
         (setq mide--rdm-executable make-ide-rdm-executable)
         mide--rdm-executable)
        ((featurep 'rtags)
         (setq mide--rdm-executable (rtags-executable-find "rdm"))
         mide--rdm-executable)
        (t "rdm")))


(defun mide--run-rc ()
  "Run rc to add definitions to the rtags daemon."
  (when (featurep 'rtags)
    (make-ide-maybe-start-rdm)
    (mide--message "Running rc for rtags")
    ;; change buffer so as to not insert text into a working file buffer
    (let ((make-ide-local-build-dir (mide--build-dir)))
      (if (get-process "rdm")
          (with-current-buffer (get-buffer make-ide-rdm-buffer-name)
            (rtags-call-rc "-J" make-ide-local-build-dir))
        (with-temp-buffer
          (rtags-call-rc "-J" make-ide-local-build-dir))))))


(defun mide--set-flags-for-file (idb buffer)
  "Set the compiler flags from IDB for BUFFER visiting file FILE-NAME."
  (let* ((file-name (mide--get-buffer-file-name buffer))
         (file-params (mide--idb-file-to-obj idb file-name))
         (sys-includes (mide--params-to-sys-includes file-params))
         (all-commands (mide--idb-all-commands idb))
         (hdr-flags (mide--commands-to-hdr-flags all-commands)))
    (mide--message "Setting flags for file %s" file-name)
    ;; set flags for all source files that registered
    (if (mide--is-src-file file-name)
        (mide--set-flags-for-src-file file-params buffer sys-includes)
      (mide--set-flags-for-hdr-file idb buffer (mide--flags-to-sys-includes hdr-flags)))))

(defun mide--get-buffer-file-name (buffer)
  "Get the name of a file for a given buffer."
  (let ((file-name (buffer-file-name buffer)))
    (mide--get-system-filename file-name)))

(defun mide--get-system-filename (file-name)
  "Get the file name considering case sensitivity of the system."
  (if (and file-name (eq system-type 'windows-nt))
      (s-downcase file-name)
    file-name))

(defun mide--set-flags-for-src-file (file-params buffer sys-includes)
  "Set the compiler flags from FILE-PARAMS for source BUFFER with SYS-INCLUDES."
  (let* ((src-flags (mide--params-to-src-flags file-params))
         (src-includes (mide--params-to-src-includes file-params)))
    (make-ide-set-compiler-flags buffer src-flags src-includes sys-includes)))

(defun mide--set-flags-for-hdr-file (idb buffer sys-includes)
  "Set the compiler flags from IDB for header BUFFER with SYS-INCLUDES."
  (when (and (not (string-empty-p (mide--buffer-string buffer))) (not make-ide-header-no-flags))
    (cond
     ;; try all unique compiler flags until one successfully compiles the header
     (make-ide-try-unique-compiler-flags-for-headers (mide--hdr-try-unique-compiler-flags idb buffer sys-includes))
     ;; ask ninja or make depending on what the user chose for the flags to use on the header
     ((mide--hdr-ask-ninja-and-make idb buffer sys-includes) t)
     ;; the default algorithm used so far
     (t (mide--hdr-legacy idb buffer sys-includes)))))

(defun mide--buffer-string (buffer)
  "Return the contents of BUFFER as a string."
  (with-current-buffer buffer
    (buffer-string)))

(defun mide--hdr-try-unique-compiler-flags (idb buffer sys-includes)
  "Try all unique compiler flags in IDB in an attempt to find appropriate flags for header file in BUFFER using SYS-INCLUDES."
  (let ((hdr-flags) (hdr-includes))
    (setq hdr-flags (mide--idb-hdr-compiler-args idb (buffer-file-name buffer)))
    (setq hdr-flags (mide--remove-compiler-from-args-string hdr-flags))
    (setq hdr-includes (mide--flags-to-includes hdr-flags))
    (make-ide-set-compiler-flags buffer hdr-flags hdr-includes sys-includes)))

(defun mide--hdr-ask-ninja-and-make (idb buffer sys-includes)
  "Try to get compiler flags from IDB from a source file that depends on the header BUFFER using SYS-INCLUDES."
  (let ((ninja-hdr-command (mide--ninja-header-command idb (buffer-file-name buffer))))
    (if ninja-hdr-command
        (progn
          (mide--set-flags-for-hdr-exact buffer sys-includes ninja-hdr-command)
          (mide--message "Setting flags for %s from ninja dependency information" (buffer-file-name buffer))
          t) ;; has done something
      nil)))

(defun mide--hdr-legacy (idb buffer sys-includes)
  "Try to set compiler flags from IDB for header BUFFER using SYS-INCLUDES.

First, try to find a source file corresponding to the header.
Then, try to find a source file in IDB that directly includes the header.
If all else fails, use all compiler flags in the project."

  (let* ((other (mide--src-file-for-hdr buffer))
         (src-file-name (or other (mide--first-including-src-file idb buffer))))
    (if src-file-name
        ;; if a source file is found, use its flags
        (mide--set-flags-for-hdr-from-src idb buffer sys-includes src-file-name)
      ;; otherwise use flags from all source files
      (mide--set-flags-for-hdr-from-all-flags idb buffer sys-includes))))

(defun mide--set-flags-for-hdr-exact (buffer sys-includes command)
  "Set flags for BUFFER using SYS-INCLUDES and compiler COMMAND."
  (let* ((hdr-flags (mide--remove-compiler-from-args-string command))
         (hdr-includes (mide--flags-to-includes hdr-flags)))
    (make-ide-set-compiler-flags buffer hdr-flags hdr-includes sys-includes)))

(defun mide--ninja-header-command (idb file-name)
  "Return the command used by a file in IDB that depends on FILE-NAME.

Find an object file that lists FILE-NAME as a dependency, then return the first
compiler command in the project that has that object file in itself."
  (let ((obj-file-name (mide--ninja-obj-file-depending-on-hdr file-name)))
    (if (null obj-file-name) nil
      (let ((all-commands (mide--idb-all-commands idb)))
        (mide--filter-first (lambda (x) (string-match obj-file-name x))
                                 all-commands)))))

(defun mide--ninja-obj-file-depending-on-hdr (file-name)
  "Find the first object file that depends on the header FILE-NAME.

Ask ninja for all dependencies then find FILE-NAME in the output, returning
the object file's name just above."
  (let ((default-directory (mide--build-dir))
        (beg)
        (end))
    (if (not (file-exists-p (expand-file-name "build.ninja" default-directory)))
        nil
      (with-temp-buffer
        (call-process make-ide-ninja-command nil t nil "-C" default-directory "-t" "deps")
        (goto-char (point-min))
        (setq beg (search-forward file-name nil t))
        (if (null beg)
            nil
          (search-backward "#deps")
          (setq beg (move-beginning-of-line nil))
          (setq end (1- (search-forward ":")))
          (copy-region-as-kill beg end)
          (car kill-ring))))))

(defun mide--src-file-for-hdr (buffer)
  "Try and find a source file for a header BUFFER (e.g. foo.cpp for foo.hpp)."
  (if make-ide-header-search-other-file
      (when (and buffer (buffer-live-p buffer))
        (with-current-buffer buffer
          (let ((other-file-name (ff-other-file-name)))
            (if other-file-name (expand-file-name other-file-name) nil))))
    nil))

(defun mide--set-flags-for-hdr-from-src (idb buffer sys-includes src-file-name)
  "Use IDB to set flags for a header BUFFER with SYS-INCLUDES from its corresponding SRC-FILE-NAME."
  (mide--message "Found src file %s for %s, using its flags" src-file-name (buffer-file-name buffer))
  (mide--set-flags-for-src-file (mide--idb-file-to-obj idb src-file-name) buffer sys-includes))

(defun mide--first-including-src-file (idb buffer)
  "Use IDB to find first source file that includes the header BUFFER."
  (when (and (buffer-file-name buffer) make-ide-header-search-first-including)
    (mide--message "Searching for source file including %s" (buffer-file-name buffer))
    (let* ((file-name (buffer-file-name buffer))
           ret-obj
           ret-file-name)

      (when (featurep 'rtags)
        (setq ret-file-name
              (with-temp-buffer
                (rtags-call-rc "--dependencies" file-name "included-by" :noerror t)
                (mide--filter-first
                 (lambda (a)
                   (gethash a idb))
                 (split-string (buffer-string) "\n" t split-string-default-separators)))))

      (unless ret-file-name
        (setq idb (mide--idb-sorted-by-file-distance idb file-name))
        (setq ret-obj (mide--filter-first
                       (lambda (x) (mide--idb-obj-depends-on-file x file-name))
                       idb))
        (when ret-obj (setq ret-file-name (mide--idb-obj-get ret-obj 'file))))

      (when ret-file-name (mide--message "Found a source file including %s" file-name))

      ret-file-name)))

(defun mide--get-string-from-file (path)
  "Return PATH's file content."
  (if (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string))
    ""))

(defun mide--set-flags-for-hdr-from-all-flags (idb buffer sys-includes)
  "Use IDB to set flags from a header BUFFER with SYS-INCLUDES from all project source files."
  (mide--message "Could not find suitable src file for %s, using all compiler flags" (buffer-file-name buffer))
  (let* ((all-commands (mide--idb-all-commands idb))
         (hdr-flags (mide--commands-to-hdr-flags all-commands))
         (hdr-includes (mide--commands-to-hdr-includes all-commands)))
    (make-ide-set-compiler-flags buffer hdr-flags hdr-includes sys-includes)))


(defun make-ide-set-compiler-flags (buffer flags includes sys-includes)
  "Set ac-clang and flycheck variables for BUFFER from FLAGS, INCLUDES and SYS-INCLUDES."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer

      (when (featurep 'auto-complete-clang)
        (make-local-variable 'ac-clang-flags)
        (setq ac-clang-flags (mide--filter-ac-flags (mide--get-compiler-flags flags))))

      (when (featurep 'company)
        (make-local-variable 'company-clang-arguments)
        (setq company-clang-arguments (mide--filter-ac-flags (mide--get-compiler-flags flags))))

      (when (featurep 'company-c-headers)
        (make-local-variable 'company-c-headers-path-user)
        (setq company-c-headers-path-user (mide--flags-to-include-paths flags))
        (make-local-variable 'company-c-headers-path-system)
        (when sys-includes
          (setq company-c-headers-path-system (append sys-includes company-c-headers-path-system))))

      (when (and (featurep 'irony) (not (gethash (mide--build-dir) mide--cache-irony-dirs)))
        (irony-cdb-json-add-compile-commands-path (mide--locate-project-dir) (mide--comp-db-file-name))
        (puthash (mide--build-dir) t mide--cache-irony-dirs))

      (when (featurep 'semantic)
        (let ((dirs (mide--flags-to-include-paths flags)))
          (when (boundp 'mide--semantic-system-include)
            (mapc 'semantic-remove-system-include mide--semantic-system-include))
          (mapc 'semantic-add-system-include dirs)
          (setq-local mide--semantic-system-include dirs)))


      (let ((macro-regex "\\(^-std=\\|\\.o$\\|^-o$\\)"))
        (make-local-variable 'c-macro-cppflags)
        (setq c-macro-cppflags
              (mapconcat 'identity (mide--filter (lambda (x) (not (string-match macro-regex x)))
                                                      (mide--filter-ac-flags (mide--get-compiler-flags flags))) " ")))

      (when (featurep 'flycheck)
        (let* ((std-regex "^-std=")
               (include-path (append sys-includes (mide--flags-to-include-paths flags)))
               (definitions (append (mide--get-existing-definitions) (mide--flags-to-defines flags)))
               (args (mide--filter (lambda (x) (not (string-match std-regex x))) (mide--flags-filtered (mide--get-compiler-flags flags)))))
          (make-local-variable 'flycheck-clang-include-path)
          (make-local-variable 'flycheck-gcc-include-path)
          (setq flycheck-clang-include-path include-path)
          (setq flycheck-gcc-include-path include-path)

          (make-local-variable 'flycheck-clang-definitions)
          (make-local-variable 'flycheck-gcc-definitions)
          (setq flycheck-clang-definitions definitions)
          (setq flycheck-gcc-definitions definitions)

          (make-local-variable 'flycheck-clang-args)
          (make-local-variable 'flycheck-gcc-args)
          (setq flycheck-clang-args args)
          (setq flycheck-gcc-args (mide--filter-output-arg args))

          (make-local-variable 'flycheck-clang-language-standard)
          (make-local-variable 'flycheck-gcc-language-standard)
          (let* ((stds (mide--filter (lambda (x) (string-match std-regex x)) flags))
                 (repls (mapcar (lambda (x) (replace-regexp-in-string std-regex "" x)) stds)))
            (when repls
              (setq flycheck-clang-language-standard (car repls))
              (setq flycheck-gcc-language-standard (car repls))
              (unless make-ide-flycheck-cppcheck-strict-standards
                (setq repls (mapcar 'mide--make-standard-to-cppcheck-standard repls)))
              (setq repls (mide--filter 'mide--valid-cppcheck-standard-p repls))
              (when repls
                (make-local-variable 'flycheck-cppcheck-standards)
                (setq flycheck-cppcheck-standards repls))))

          (make-local-variable 'flycheck-cppcheck-include-path)
          (setq flycheck-cppcheck-include-path (append sys-includes (mide--flags-to-include-paths flags))))

        (setq flycheck-clang-includes includes)
        (setq flycheck-gcc-includes includes)
        (flycheck-clear)
        (run-at-time "0.5 sec" nil 'flycheck-buffer)))))

(defun make-ide-delete-file ()
  "Remove file connected to current buffer and kill buffer, then run Make."
  (interactive)
  (when (mide--locate-project-dir)
    (if (mide--build-dir)
        (let ((filename (buffer-file-name))
              (buffer (current-buffer))
              (name (buffer-name)))
          (if (not (and filename (file-exists-p filename)))
              (error "Buffer '%s' is not visiting a file!" name)
            (when (yes-or-no-p "Are you sure you want to remove this file? ")
              (delete-file filename)
              (kill-buffer buffer)
              (let ((project-dir (mide--locate-project-dir)))
                (when (and project-dir  (file-exists-p (expand-file-name "Makefile" project-dir)))
                  (mide--run-make-impl project-dir (mide--build-dir)))
                (mide--message "File '%s' successfully removed" filename)))))
      (error "Not possible to delete a file without setting make-ide-build-dir"))))


(defun mide--run-make-impl (project-dir make-dir)
  "Run the Make process for PROJECT-DIR in MAKE-DIR."
  (when project-dir
    (let ((default-directory make-dir))
      (mide--message "Running make for src path %s in build path %s" project-dir make-dir)
      (mide--message "Running make cmd: %s "(append (list "make" "*make*" make-ide-make-command)
                                    (mide--make-args)
                                    (list "-C" project-dir)
									(list "| rc -c -")))
      (apply 'start-process (append (list "make" "*make*" make-ide-make-command)
                                    (mide--make-args)
                                    (list "-C" project-dir)
									)))))


(defun mide--project-key ()
  "Return a unique key for a project based on the project dir and make options."
  (let ((project-dir (mide--locate-project-dir)))
    (when project-dir
      ;; if no project-dir, then get-project-key is called from a non make project dir, simply ignore
      (replace-regexp-in-string "[-/= ]" "_"  (concat (expand-file-name project-dir)
                                                      (string-join (mide--make-args) " "))))))

(defun mide--make-args ()
  "Return a list of arguments to pass to Make when calling it."
  (or make-ide-make-args make-ide-make-args (split-string make-ide-make-opts)))

(defun mide--build-dir ()
  "Return the directory name to run Make in."
  (let ((build-dir-base
         (or (mide--build-dir-var) ; if use set, use this value (may be relative)
             (mide--build-dir-from-cache)))) ; else get from project-key (return an absolute path)
    (when build-dir-base
      (let ((build-dir (expand-file-name  build-dir-base
                                          (mide--locate-project-dir)))) ; if relative, use project-dir as base directory
        (when (not (file-accessible-directory-p build-dir))
          (mide--message "Making directory %s" build-dir)
          (make-directory build-dir 't))
        (file-name-as-directory build-dir)))))

(defun mide--build-dir-from-cache ()
  "Get the build dir from the cache if there or compute if not.
Return nil for non-Make project."
  (let ((project-key (mide--project-key)))
    (when project-key
      (let ((build-dir (gethash project-key mide--cache-pkey-to-dir nil)))
        (or build-dir
            (let ((build-parent-directory (or make-ide-build-pool-dir temporary-file-directory))
                  (build-directory-name (if make-ide-build-pool-use-persistent-naming
                                            project-key
                                          (make-temp-name "make"))))
              (setq build-dir (expand-file-name build-directory-name build-parent-directory))
              (progn
                (puthash project-key build-dir mide--cache-pkey-to-dir))
              build-dir))))))


(defun mide--is-src-file (name)
  "Test if NAME is a source file or not."
  (cl-some (lambda (x) (string-suffix-p x name)) make-ide-src-extensions))


(defun mide--filter (pred seq)
  "Apply PRED to filter SEQ."
  (delq nil
        (mapcar (lambda (x) (and (funcall pred x) x)) seq)))

(defun mide--filter-first (pred seq)
  "Return first element to satisfy PRED in SEQ."
  (let ((index 0) (ret))
    (while (and (null ret) (< index (length seq)))
      (when (funcall pred (elt seq index))
        (setq ret (elt seq index)))
      (cl-incf index))
    ret))


(defun mide--filter-params (file-params filter-func)
  "Filter FILE-PARAMS with FILTER-FUNC."
  ;; The compilation database is a JSON array of JSON objects
  ;; Each object is a file with directory, file and command fields
  ;; Depending on FILTER-FUNC, it maps file names to desired compiler flags
  ;; An example would be -I include flags
  (let* ((args (mide--file-params-to-args file-params))
         (flags (funcall filter-func args)))
    (mapconcat 'identity flags " ")))

(defun mide--file-params-to-args (file-params)
  "Get the compiler command arguments from FILE-PARAMS."
  (let ((command (mide--idb-obj-get file-params 'command))
        (arguments (mide--idb-obj-get file-params 'arguments)))
    (mide--resolve-response-file
     (if command
         (mapcar #'mide--quote-if-spaces (mide--split-command command))
       (mide--vector-to-list arguments)))))

(defun mide--resolve-response-file (argument-list)
  "Matches response file string and adds its content to the object parameters."
  (-flatten (mapcar #'mide--replace-response-file argument-list)))

(defun mide--replace-response-file (argument)
  "Matches a response file in string ARGUMENT returning a list of arguments"
  (if (not (stringp argument))
      argument
    (if (string-match "@[^[:space:]]+" argument)
        (let* ((response-file (substring argument 1))
               (file-params (mide--get-file-params response-file)))
          (mapcar #'mide--quote-if-spaces (mide--split-command file-params)))
      argument)))

(defun mide--get-file-params (response-file)
  "Get file parameters from a response file given as compilation argument."
  (replace-regexp-in-string "\\\n" " " (mide--get-string-from-file (expand-file-name response-file (mide--build-dir)))))

(defun mide--quote-if-spaces (str)
  "Add quotes to STR if it has spaces."
  (if (string-match-p " " str)
      (concat "\"" str "\"")
    str))

(defun mide--vector-to-list (vector)
  "Convert VECTOR to a list."
  (append vector nil))

(defun mide--args-to-only-flags (args)
  "Get compiler flags from ARGS."
  (mide--filter (lambda (x) (not (mide--is-src-file (mide--unquote x)))) args))

(defun mide--unquote (x)
  "Possibly unquote a string X."
  (if (and (stringp x) (> (length x) 2))
      (if (and (equal (elt x 0) ?\") (equal (elt x (1- (length x))) ?\"))
          (cl-subseq x 1 (1- (length x)))
        x)
    x))

(defun mide--json-unescape (str)
  "Remove JSON-escaped backslashes in STR."
  (let* ((no-double-backslashes (replace-regexp-in-string "\\\\\\\\" "\\\\" str))
         (no-backslash-quote (replace-regexp-in-string "\\\\\"" "\"" no-double-backslashes)))
    no-backslash-quote))

(defun mide--params-to-src-flags (file-params &optional filter-func)
  "Source compiler flags for FILE-PARAMS using FILTER-FUNC."
  (if (not file-params) nil
    (let* ((filter-func (or filter-func #'mide--args-to-only-flags))
           (value (mide--filter-params file-params filter-func))
           (flags-string (if value value nil)))
      (if flags-string (mide--cleanup-flags-str flags-string) nil))))

(defun mide--cleanup-flags-str (str)
  "Clean up and filter STR to yield a list of compiler flags."
  (let ((unescaped-flags-string (mide--json-unescape str)))
    (mide--remove-compiler-from-args-string unescaped-flags-string)))

(defun mide--remove-compiler-from-args-string (str)
  "Remove the compiler command from STR, leaving only arguments.  Return a list of strings."
  (let ((args (mide--split-command str)))
    (mide--remove-compiler-from-args args)))

(defun mide--remove-compiler-from-args (args)
  "Remove the compiler command from ARGS, leaving only arguments."
  (if (string-suffix-p "ccache" (car args))
      (cddr args)
    (cdr args)))


(defun mide--filter-ac-flags (flags)
  "Filter unwanted compiler arguments out from FLAGS."
  (mide--filter
   (lambda (x)
     (cl-loop for flag in make-ide-ac-flags-to-filter
              never (string-match (format "^%s$" flag) x)))
   flags))

(defun mide--delete-dup-hdr-flags (flags)
  "Delete duplicates in FLAGS for header files."
  (let* ((rest (mide--flags-filtered flags))
         (dashes (mide--filter #'mide--dash-i-or-dash-d-p flags)))
    (append (delete-dups dashes) rest)))

(defun mide--commands-to-hdr-flags (commands)
  "Header compiler flags from COMMANDS."
  (let* ((args (mide--flatten (mapcar #'mide--remove-compiler-from-args-string commands)))
         (flags (mide--args-to-only-flags args)))
    (setq flags (mide--filter (lambda (x) (not (equal x "-o"))) flags))
    (setq flags (mide--filter (lambda (x) (not (string-suffix-p ".o" x))) flags))
    (setq flags (mide--filter (lambda (x) (not (string-suffix-p ".obj" x))) flags))
    (mide--delete-dup-hdr-flags flags)))

(defun mide--params-to-src-includes (file-params)
  "-include compiler flags for from FILE-PARAMS."
  (mide--flags-to-includes (mide--params-to-src-flags file-params 'identity)))


(defun mide--params-to-sys-includes (file-params)
  "-include compiler flags for from FILE-PARAMS."
  (mide--flags-to-sys-includes (mide--params-to-src-flags file-params 'identity)))


(defun mide--commands-to-hdr-includes (commands)
  "Header `-include` flags from COMMANDS."
  (let ((args (mide--flatten (mapcar #'mide--remove-compiler-from-args-string commands))))
    (delete-dups (mide--flags-to-includes args))))


(defun mide--flatten (lst)
  "Flatten LST."
  (if (equal (length lst) 1)
      (elt lst 0)
    (apply #'append lst)))


(defun mide--flags-to-include-paths (flags)
  "From FLAGS (a list of flags) to a list of include paths."
  (let ((raw-paths (mide--to-simple-flags flags "^-I")))
    (mapcar (lambda (x) (expand-file-name x (mide--build-dir))) raw-paths)))

(defun mide--relativize (path)
  "Make PATH relative to the build directory, but only if relative path with dots."
  (if (or (equal path ".") (string-prefix-p ".." path))
      (expand-file-name path (mide--build-dir))
    path))


(defun mide--flags-to-defines (flags)
  "From FLAGS (a list of flags) to a list of defines."
  (mide--to-simple-flags flags "^-D"))


(defun mide--flags-to-includes (flags)
  "From FLAGS (a list of flags) to a list of includes."
  (let ((includes nil))
    (while (member "-include" flags)
      (setq flags (cdr (member "-include" flags)))
      (when flags (setq includes (cons (car flags) includes))))
    includes))

(defun mide--flags-to-sys-includes (flags)
  "From FLAGS (a list of flags) to a list of isystem includes."
  (let ((sysincludes nil))
    (while (member "-isystem" flags)
      (setq flags (cdr (member "-isystem" flags)))
      (when flags
        (if (member (car flags) sysincludes)
            nil
          (setq sysincludes (cons (car flags) sysincludes)))))
    sysincludes))


(defun mide--dash-i-or-dash-d-p (flag)
  "If FLAG is -I or -D."
  (let* ((case-fold-search nil)
         (imatch (string-match "^-I" flag))
         (dmatch (string-match "^-D" flag)))
    (or imatch dmatch)))

(defun mide--flags-filtered (flags)
  "Filter out defines and includes from FLAGS."
  (mide--filter (lambda (x) (not (mide--dash-i-or-dash-d-p x))) flags))


(defun mide--to-simple-flags (flags flag)
  "A list of either directories or defines from FLAGS depending on FLAG."
  (let* ((case-fold-search nil)
         (res-flags (mide--filter
                     (lambda (x)
                       (let ((match (string-match flag x)))
                         (and match (zerop match))))
                     flags)))
    (mapcar (lambda (x) (replace-regexp-in-string flag "" x)) res-flags)))


(defun mide--get-compiler-flags (flags)
  "Use FLAGS to return all compiler flags including existing ones."
  (append (mide--get-existing-compiler-flags) flags))

(defun mide--get-existing-compiler-flags ()
  "Return existing ac-clang flags for this mode, if set."
  (if (eq major-mode 'c++-mode)
      (mide--symbol-value 'make-ide-flags-c++)
    (mide--symbol-value 'make-ide-flags-c)))

(defun mide--get-existing-definitions ()
  "Return existing compiler defines, if set."
  (mide--symbol-value 'make-ide-definitions))


(defun mide--symbol-value (sym)
  "Return the value of SYM if bound, nil if not."
  (if (boundp sym) (symbol-value sym) nil))


(defun mide--locate-makelists ()
  "Find Makefile

Use MakeLists.txt in user defined project-dir, or find the topmost
MakeLists.txt file.  Return nil if not found."
  (if (and (mide--project-dir-var)
           (file-exists-p (expand-file-name "Makefile" (mide--project-dir-var))))
      (expand-file-name "Makefile" (mide--project-dir-var))
    (let ((makelist-dir (mide--topmost-makelists default-directory nil)))
      (if makelist-dir
          (expand-file-name "Makefile" makelist-dir)
        nil))))

(defun mide--topmost-makelists (dir last-found)
  "Find the topmost MakeLists.txt from DIR using LAST-FOUND as a 'plan B'."
  (let ((new-dir (locate-dominating-file dir "Makefile")))
    (if new-dir
        (mide--topmost-makelists (expand-file-name ".." new-dir) new-dir)
      last-found)))

(defun mide--locate-project-dir ()
  "Return the path to the project directory."
  (let ((makelists (mide--locate-makelists)))
    ;; if project dir is set by the user, use this value.
    (or (and (mide--project-dir-var) (expand-file-name (mide--project-dir-var)))
        (and makelists (file-name-directory makelists)) ; else try to use makelists dir
        nil ; if no Makefile nor project-dir set, return nil and prevent make-ide to do anything else
        )))

(defun mide--cdb-json-file-to-idb ()
  "Convert the compile_commands.json CDB to an IDB.
First it checks the cache for previously
computed IDBs, and if none are found actually performs the conversion."
)

(defun mide--cdb-idb-from-cache ()
  "Return the IDB from the cache unless the JSON CDB has changed."
  (let ((idb (gethash (mide--build-dir) mide--cache-dir-to-idb))
        (cached-hash (gethash (mide--build-dir) mide--cache-dir-to-cdb-hash))
        (current-hash (mide--hash-file (mide--comp-db-file-name))))
    (if (equal cached-hash current-hash)
        idb
      nil)))

(defun mide--hash-file (file-name)
  "Calculate the hash of FILE-NAME."
  (secure-hash 'md5 (mide--get-string-from-file file-name)))

(defun mide--cdb-json-string-to-idb (json-str)
  "Tranform JSON-STR into an IDB.

The IDB is hash mapping files to all JSON objects (usually only one) in the CDB."
  (let ((idb (mide--make-hash-table))
        (json (json-read-from-string json-str)))
    ;; map over all the JSON objects in JSON, which is an array of objects (CDB)
    (mapc (lambda (obj)
            (let* ((file (mide--get-system-filename (mide--relativize (mide--idb-obj-get obj 'file))))
                   (objs (gethash file idb)))
              (push obj objs)
              (puthash file objs idb)))
          json)
    idb))

(defun mide--idb-obj-get (obj key)
  "Get the value in OBJ for KEY."
  (cdr (assoc key obj)))

(defmacro mide--idb-obj-set (obj key value)
  "Take OBJ and set KEY to VALUE."
  `(push (cons ,key ,value) ,obj))

(defun mide--idb-file-to-obj (idb file-name)
  "Get object from IDB for FILE-NAME."
  (car (gethash file-name idb)))

(defun mide--idb-all-commands (idb)
  "A list of all commands in IDB."
  (mapcar (lambda (x) (s-join " " (mide--file-params-to-args x))) (mide--idb-all-objs idb)))


(defun mide--idb-sorted-by-file-distance (idb file-name)
  "Return a list of IDB entries sorted by their directory's name's distance to FILE-NAME."
  (let ((dir (file-name-directory file-name))
        (ret))

    (setq ret (mapcar (lambda (x) (push `(distance . ,(mide--file-distance dir x)) x)) (mide--idb-all-objs idb)))

    (seq-sort
     (lambda (x y) (< (mide--idb-obj-get x 'distance)
                      (mide--idb-obj-get y 'distance)))
     ret)))

(defun mide--file-distance (dir object)
  "Return the distance between DIR and OBJECT's file."
  (levenshtein-distance dir (file-name-directory (mide--idb-obj-get object 'file))))

(defun mide--idb-all-objs (idb)
  "Return a list of IDB entries."
  (let ((ret))
    (maphash (lambda (_ objs) (setq ret (append ret objs))) idb)
    ret))


(defun mide--idb-obj-depends-on-file (obj file-name)
  "If OBJ is a source file that depends on FILE-NAME."
  (let* ((base-name (file-name-nondirectory file-name))
         (src-file-name (mide--idb-obj-get obj 'file)))
    (if (string-match (concat "# *include +[\"<] *" base-name)
                      (mide--get-string-from-file src-file-name))
        src-file-name
      nil)))

(defun mide--idb-hdr-compiler-args (idb file-name)
  "Try every unique compiler command in IDB on FILE-NAME and return the first to succeed."
  (let* ((objects  (mide--idb-sorted-by-file-distance idb file-name))
         (commands (mide--idb-objs-to-unique-commands objects))
         (index 0)
         (ret))
    (while (and (null ret) (< index (length commands)))
      (let* ((tmp-file-name (expand-file-name "tmp.o" (make-temp-file "tryheader" t)))
             (command (concat (elt commands index) " " file-name " " "-o" " " tmp-file-name))
             (_ (mide--message "Trying to compile '%s' with '%s'" file-name command))
             (args (mide--split-command command)))
        (when (eq 0 (apply #'call-process (car args) nil nil nil (cdr args)))
          (setq ret command)))
      (cl-incf index))
    ret))


(defun mide--idb-objs-to-unique-commands (objects)
  "Calculate the list of unique compiler commands in OBJECTS ignoring the source file name."
  (let ((ret (mapcar (lambda (x)
                       (let* ((file (mide--idb-obj-get x 'file))
                              (base-name (file-name-nondirectory file))
                              (command (mide--idb-obj-get x 'command))
                              (args (mide--split-command command)))
                         (setq args (mide--filter (lambda (x) (not (string-match base-name x))) args))
                         (setq args (mide--filter (lambda (x) (not (equal x "-c"))) args))
                         (setq args (mide--filter (lambda (x) (not (equal x "-o"))) args))
                         (mapconcat 'identity args " ")))
                     objects)))
    (delete-dups ret)
    ret))

(defun mide--split-command (command-string)
  "Split COMMAND-STRING and return a list of strings."
  (split-string-and-unquote (replace-regexp-in-string "\\\\\"" "\"" command-string)))

;;;###autoload
(defun make-ide-maybe-start-rdm ()
  "Start the rdm (rtags) server."
  (interactive)
  (when (and (featurep 'rtags)
             (or (and (mide--comp-db-file-name) (file-exists-p (mide--comp-db-file-name)))
                 (mide--locate-project-dir)))

    (unless (mide--process-running-p "rdm")
      (let ((buf (get-buffer-create make-ide-rdm-buffer-name)))
        (mide--message "Starting rdm server")
        (with-current-buffer buf
          (let ((rdm-process (start-process "rdm" (current-buffer)
                                            (make-ide-rdm-executable)
                                            "-c" make-ide-rdm-rc-path)))
                                        ; add a small delay before going on, since rdm could take some time to be ready to treat rc commands
            (sleep-for 0.8)
            (set-process-query-on-exit-flag rdm-process nil)))))))


(defun mide--process-running-p (name)
  "If a process called NAME is running or not."
  (or (get-process name) (mide--system-process-running-p name)))

(defun mide--system-process-running-p (name)
  "If a process called NAME is running on the system."
  (let* ((all-args (mapcar (lambda (x) (cdr (assq 'args (process-attributes x)))) (list-system-processes)))
         (match-args (mide--filter (lambda (x) (mide--string-match (concat "\\b" name "\\b") x)) all-args)))
    (not (null match-args))))

(defun mide--string-match (regexp name)
  "Wrap 'string-match' of REGEXP and NAME to make sure we don't pass it a nil string."
  (when name
    (string-match regexp name)))

(defun mide--valid-cppcheck-standard-p (standard)
  "If STANDARD is supported by cppcheck."
  (member standard '("posix" "c89" "c99" "c11" "c++03" "c++11")))

(defun mide--make-standard-to-cppcheck-standard (standard)
  "Convert a Make language STANDARD to the closest supported by cppcheck.
If there is no clear and sensible conversion, the input is
returned unchanged."
  (let ((gnu-replaced (replace-regexp-in-string "gnu" "c" standard)))
    (cond
     ;; Convert "close-enough" matches.
     ((equal gnu-replaced "c90") "c89")
     ((equal gnu-replaced "c++98") "c++03")
     ((equal gnu-replaced "c++0x") "c++03")
     ((equal gnu-replaced "c++14") "c++11")
     ((equal gnu-replaced "c++1y") "c++11")
     ((equal gnu-replaced "c++17") "c++11")
     ((equal gnu-replaced "c++1z") "c++11")
     ;; See if what we have matches cppcheck's capabilities exactly.
     ((mide--valid-cppcheck-standard-p gnu-replaced) gnu-replaced)
     ;; Otherwise, just hand back the original input.
     (t standard))))

(defun mide--filter-output-arg (args)
  "Filter out '-o <output>' from the provided 'args' list."
  (let (result)
      (while args
        (if (string-equal "-o" (car args))
            (setq args (nthcdr 2 args))
          (push (car args) result)
          (setq args (cdr args))))
      (nreverse result)))

(provide 'make-ide)
;;; make-ide.el ends here
