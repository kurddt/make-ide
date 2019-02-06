;;; utils-test.el --- Unit tests for cmake-ide.

;; Copyright (C) 2014-2018

;; Author:  <atila.neves@gmail.com>
;; Keywords:

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

;;

;;; Code:

(require 'f)

(defvar cide--test-path)
(defvar cide--root-path)
(setq cide--test-path (f-dirname load-file-name))
(setq cide--root-path (f-parent cide--test-path))
(add-to-list 'load-path cide--root-path)

(require 'ert)
(require 'cl)
(require 'make-ide)


(ert-deftest test-cide--string-empty-p ()
  (should (equal (cide--string-empty-p "") t))
  (should (equal (cide--string-empty-p "foo") nil)))

(ert-deftest test-unquote ()
  (should (equal (cide--unquote "\"foo\"") "foo")))

(ert-deftest test-cide--string-match ()
  (should (equal (cide--string-match "foo" "foobar") 0))
  (should (equal (cide--string-match "oo" "foobar") 1))
  (should (equal (cide--string-match "foo" "bar") nil)))

(ert-deftest test-cide--idb-objs-to-unique-commands ()
  (should (equal (cide--idb-objs-to-unique-commands
                  '(((file . "foo") (command . "a b c"))
                    ((file . "bar") (command . "d e f"))))
                 '("a b c" "d e f")))
  (should (equal (cide--idb-objs-to-unique-commands
                  '(((file . "foo") (command . "a b c"))
                    ((file . "bar") (command . "a b c"))))
                 '("a b c"))))

(ert-deftest test-cide--idb-all-objs ()
  (let ((idb (make-hash-table)))
    (puthash "foo" '(((file . "foo") (command . "cmd1"))) idb)
    (puthash "bar" '(((file . "bar") (command . "cmd2"))) idb)
    (should (equal (cide--idb-all-objs idb)
                   '(((file . "foo") (command . "cmd1"))
                     ((file . "bar") (command . "cmd2")))))))

(ert-deftest test-filter-first ()
  (should (equal (cide--filter-first (lambda (x) (equal x "foo")) '("bar" "foo")) "foo"))
  (should (equal (cide--filter-first (lambda (x) (equal x "quux")) '("bar" "foo")) nil)))

(ert-deftest test-get-buffer-file-name ()
  (cl-letf (((symbol-function 'cide--get-system-filename) #'(lambda (filename) filename)))
    (let* ((utils-test-buffer (find-file "utils-test.el"))
           (utils-test-filename (buffer-file-name (get-buffer "utils-test.el"))))
      (should (equal (cide--get-buffer-file-name utils-test-buffer) utils-test-filename))))
  (should-error (test-get-buffer-file-name nil)))

(ert-deftest test-get-system-filename ()
  (let ((initial-system-type system-type))
    (should
     (progn
       (make-local-variable system-type)
       (setq system-type 'windows-nt)
       (equal (cide--get-system-filename "C:/MyTestPath/MyFile.cpp") "c:/mytestpath/myfile.cpp")
       (setq system-type initial-system-type)))
    (should
     (progn
       (make-local-variable system-type)
       (setq system-type 'any-other-system)
       (equal (cide--get-system-filename "C:/MyTestPath/MyFile.cpp") "C:/MyTestPath/MyFile.cpp")
       (setq system-type initial-system-type)))))

(ert-deftest test-get-file-params ()
  (let ((temporary-filename (make-temp-file "test-get-file-params"))
        (cide--build-dir-called nil)
        (return-value nil))
    (cl-letf (((symbol-function 'cide--build-dir) #'(lambda () (setq cide--build-dir-called t return-value nil))))
      (with-temp-file temporary-filename
        (insert "-fmessage-length=0")
        (end-of-line)
        (newline)
        (insert "-nostdlib")
        (end-of-line)
        (newline))
      (should (equal (cide--get-file-params temporary-filename) "-fmessage-length=0 -nostdlib "))
      (should (equal cide--build-dir-called t)))
    (delete-file temporary-filename))
  (should-error (cide--get-file-params nil)))

(ert-deftest test-replace-response-file ()
  (cl-letf (((symbol-function 'cide--build-dir) #'(lambda () nil)))
    (let ((temporary-filename (make-temp-file "test-get-file-params")))
      (with-temp-file temporary-filename
        (insert "-fmessage-length=0 -nostdlib")
        (end-of-line)
        (newline))
      (should (equal (cide--replace-response-file (concat "@" temporary-filename)) (list "-fmessage-length=0" "-nostdlib")))
      (delete-file temporary-filename)))
  )

(ert-deftest test-resolve-response-file ()
  (cl-letf (((symbol-function 'cide--build-dir) #'(lambda () nil)))
    (let ((temporary-filename (make-temp-file "test-get-file-params")))
      (with-temp-file temporary-filename
        (insert "-fmessage-length=0 -nostdlib")
        (end-of-line)
        (newline))
      (should (equal (cide--resolve-response-file (list "-Iinclude" (concat "@" temporary-filename))) (list "-Iinclude" "-fmessage-length=0" "-nostdlib")))
      (delete-file temporary-filename)))
  )


(ert-deftest test-flags-to-sys-includes ()
  (let ((flags '("-isystem" "foo" "quux" "toto" "-isystem" "bar" "haha")))
    (should (equal (cide--flags-to-sys-includes flags)
                   '("bar" "foo")))))

(provide 'utils-test)
;;; utils-test.el ends here
