;;; gimei.el --- Random Japanese name generator

;; Author: Wataru MIYAGUNI <gonngo@gmail.com>
;; URL: https://github.com/gongo/emacs-gimei
;; Version: 0.0.1
;; Keywords: gimei faker

;; Copyright (c) 2015 Wataru MIYAGUNI
;;
;; MIT License
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; Emacs Lisp port of https://github.com/willnet/gimei

;;; Code:

(require 'cl-lib)

(defvar gimei->names nil)

(defvar gimei->names-data-path
  (let ((current-dir (file-name-directory (or load-file-name (buffer-file-name)))))
    (concat current-dir "gimei-data")))

(cl-defstruct gimei:name
  firstname
  lastname)

(defun gimei:first:kanji (gn)
  (nth 0 (gimei:name-firstname gn)))

(defun gimei:first:hiragana (gn)
  (nth 1 (gimei:name-firstname gn)))

(defun gimei:first:katakana (gn)
  (nth 2 (gimei:name-firstname gn)))

(defun gimei:last:kanji (gn)
  (nth 0 (gimei:name-lastname gn)))

(defun gimei:last:hiragana (gn)
  (nth 1 (gimei:name-lastname gn)))

(defun gimei:last:katakana (gn)
  (nth 2 (gimei:name-lastname gn)))

(cl-defun gimei:kanji (gn &optional (delim " "))
  (concat (gimei:last:kanji gn)
          delim
          (gimei:first:kanji gn)))

(cl-defun gimei:hiragana (gn &optional (delim " "))
  (concat (gimei:last:hiragana gn)
          delim
          (gimei:first:hiragana gn)))

(cl-defun gimei:katakana (gn &optional (delim " "))
  (concat (gimei:last:katakana gn)
          delim
          (gimei:first:katakana gn)))

(defun gimei--load-data ()
  (unless gimei->names
    (load-library gimei->names-data-path)))

(defun gimei:new-name ()
  (if (zerop (random 2))
      (gimei:new-male)
    (gimei:new-female)))

(defun gimei:new-male ()
  (gimei--load-data)
  (let ((first-names (cdr (assoc "male" (assoc "first-name" gimei->names))))
        (last-names (cdr (assoc "last-name" gimei->names))))
    (make-gimei:name
     :firstname (nth (random (length first-names)) first-names)
     :lastname (nth (random (length last-names)) last-names))))

(defun gimei:new-female ()
  (gimei--load-data)
  (let ((first-names (cdr (assoc "female" (assoc "first-name" gimei->names))))
        (last-names (cdr (assoc "last-name" gimei->names))))
    (make-gimei:name
     :firstname (nth (random (length first-names)) first-names)
     :lastname (nth (random (length last-names)) last-names))))

(provide 'gimei)
