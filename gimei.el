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

(defvar gimei->data-path
  (let ((current-dir (file-name-directory (or load-file-name (buffer-file-name)))))
    (concat current-dir "gimei-data")))

(cl-defstruct gimei:name
  firstname
  lastname)

(cl-defstruct gimei:address
  prefecture
  city
  town)

(defun gimei:first:kanji-of (gn)
  (nth 0 (gimei:name-firstname gn)))

(defun gimei:first:hiragana-of (gn)
  (nth 1 (gimei:name-firstname gn)))

(defun gimei:first:katakana-of (gn)
  (nth 2 (gimei:name-firstname gn)))

(defun gimei:last:kanji-of (gn)
  (nth 0 (gimei:name-lastname gn)))

(defun gimei:last:hiragana-of (gn)
  (nth 1 (gimei:name-lastname gn)))

(defun gimei:last:katakana-of (gn)
  (nth 2 (gimei:name-lastname gn)))

(defun gimei:prefecture:kanji-of (ga)
  (nth 0 (gimei:address-prefecture ga)))

(defun gimei:prefecture:hiragana-of (ga)
  (nth 1 (gimei:address-prefecture ga)))

(defun gimei:prefecture:katakana-of (ga)
  (nth 2 (gimei:address-prefecture ga)))

(defun gimei:city:kanji-of (ga)
  (nth 0 (gimei:address-city ga)))

(defun gimei:city:hiragana-of (ga)
  (nth 1 (gimei:address-city ga)))

(defun gimei:city:katakana-of (ga)
  (nth 2 (gimei:address-city ga)))

(defun gimei:town:kanji-of (ga)
  (nth 0 (gimei:address-town ga)))

(defun gimei:town:hiragana-of (ga)
  (nth 1 (gimei:address-town ga)))

(defun gimei:town:katakana-of (ga)
  (nth 2 (gimei:address-town ga)))

(cl-defun gimei:kanji-of (gn &optional (delim " "))
  (concat (gimei:last:kanji-of gn)
          delim
          (gimei:first:kanji-of gn)))

(cl-defun gimei:hiragana-of (gn &optional (delim " "))
  (concat (gimei:last:hiragana-of gn)
          delim
          (gimei:first:hiragana-of gn)))

(cl-defun gimei:katakana-of (gn &optional (delim " "))
  (concat (gimei:last:katakana-of gn)
          delim
          (gimei:first:katakana-of gn)))

(cl-defun gimei:address:kanji-of (ga &optional (delim ""))
  (concat (gimei:prefecture:kanji-of ga)
          delim
          (gimei:city:kanji-of ga)
          delim
          (gimei:town:kanji-of ga)))

(cl-defun gimei:address:hiragana-of (ga &optional (delim ""))
  (concat (gimei:prefecture:hiragana-of ga)
          delim
          (gimei:city:hiragana-of ga)
          delim
          (gimei:town:hiragana-of ga)))

(cl-defun gimei:address:katakana-of (ga &optional (delim ""))
  (concat (gimei:prefecture:katakana-of ga)
          delim
          (gimei:city:katakana-of ga)
          delim
          (gimei:town:katakana-of ga)))

(cl-defun gimei:kanji (&optional (delim ""))
  (gimei:kanji-of (gimei:new-name) delim))

(cl-defun gimei:hiragana (&optional (delim ""))
  (gimei:hiragana-of (gimei:new-name) delim))

(cl-defun gimei:katakana (&optional (delim ""))
  (gimei:katakana-of (gimei:new-name) delim))

(cl-defun gimei:address:kanji (&optional (delim ""))
  (gimei:address:kanji-of (gimei:new-address) delim))

(cl-defun gimei:address:hiragana (&optional (delim ""))
  (gimei:address:hiragana-of (gimei:new-address) delim))

(cl-defun gimei:address:katakana (&optional (delim ""))
  (gimei:address:katakana-of (gimei:new-address) delim))

(defun gimei--load-data ()
  (unless gimei->names
    (load-library gimei->data-path)))

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

(defun gimei:new-address ()
  (gimei--load-data)
  (let ((prefectures (cdr (assoc "prefecture" gimei->addresses)))
        (cities (cdr (assoc "city" gimei->addresses)))
        (towns (cdr (assoc "town" gimei->addresses))))
    (make-gimei:address
     :prefecture (nth (random (length prefectures)) prefectures)
     :city (nth (random (length cities)) cities)
     :town (nth (random (length towns)) towns))))

(provide 'gimei)
