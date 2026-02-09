;;; test.lisp - test suite
;;;
;;; Copyright (c) 2022 Samuel Hunter
;;; This software is licensed under the MIT License.
;;; See LICENSE for details.

(defpackage #:xyz.shunter.input-event-codes.test
  (:use #:cl)
  (:local-nicknames (#:p #:org.shirakumo.parachute)))

(in-package #:xyz.shunter.input-event-codes.test)



(defun earmuffed-p (sym)
  (let ((name (symbol-name sym)))
    (char= #\+
           (char name 0)
           (char name (1- (length name))))))

;; There's no behavior -- this is literally just a constants collection -- so
;; this test is just thumbing through the symbols making sure they're all
;; properly-named constant fixnums.
(p:define-test all-syms-are-fixnum-constants
  (do-external-symbols (sym :xyz.shunter.input-event-codes)
    (p:true (and (earmuffed-p sym)
                 (constantp sym)
                 (typep (symbol-value sym) 'fixnum)))))
