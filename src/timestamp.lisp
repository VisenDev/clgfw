(in-package #:clgfw)

;;; TIMESTAMPS
(declaim (ftype (function () integer) timestamp-get))
(defun timestamp-get ()
  (get-internal-real-time))

(declaim (ftype (function (integer integer) integer) timestamp-difference))
(defun timestamp-difference (start end)
  (- end start))

(defun timestamp-difference-seconds (start end)
  (/ (timestamp-difference start end)
     internal-time-units-per-second))
