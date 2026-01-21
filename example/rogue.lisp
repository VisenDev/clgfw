(defpackage #:clgfw/example/rogue
  (:use #:cl)
  (:export #:main))
(in-package #:clgfw/example/rogue)

(defgeneric render (ctx level entity))
(defgeneric update (ctx level entity))

(defclass entity ()
  ((x :accessor x :initarg :x :initform 0)
   (y :accessor y :initarg :y :initform 0))
  )
(defmethod render (ctx level (entity entity))
  "Empty default renderer"
  (declare (ignore entity ctx level)))
(defmethod update (ctx level (entity entity))
  "Empty default updater"
  (declare (ignore entity ctx level)))

(defclass killable (entity)
  ((hp :accessor hp :initarg :hp)))

;;; ==== PLAYER ====
(defclass player (entity) ())
(defmethod update (ctx level (player player))
  (when (clgfw:is-key-pressed ctx :j)
    (incf (y player)))
  (when (clgfw:is-key-pressed ctx :k)
    (decf (y player)))
  (when (clgfw:is-key-pressed ctx :h)
    (decf (x player)))
  (when (clgfw:is-key-pressed ctx :l)
    (incf (x player)))
  )

(defparameter *red* nil)
(defmethod render (ctx level (player player))
  (unless *red*
    (setf *red* (clgfw:make-color :r 200 :g 10 :b 10)))
  (let ((sz (tilesize level)))
    (clgfw:draw-rectangle
     ctx
     (* sz (x player))
     (* sz (y player))
     sz sz
     *red*)))

(defclass level ()
  ((tilesize :accessor tilesize :initform 40)
   (map :accessor tilemap :initarg :map)
   (entities :accessor entities :initarg :entities)))

(defun create-map ()
    #(#(0 0 0 0 0 0 0 0 0 0 0 0 0)
      #(0 1 1 1 0 1 0 0 0 0 0 0 0)
      #(0 1 1 1 1 1 1 1 0 0 0 0 0)
      #(0 1 0 0 1 0 0 1 0 1 1 1 0)
      #(0 1 0 0 0 0 0 1 1 1 1 1 0)
      #(0 1 0 0 0 0 0 0 0 1 1 1 0)
      #(0 1 0 0 0 0 0 0 0 0 0 0 0)
      #(0 1 0 0 0 0 0 0 0 0 0 0 0)
      #(0 1 0 0 0 0 0 0 0 0 0 0 0)
      #(0 0 0 0 0 0 0 0 0 0 0 0 0)))

(defparameter *solid* nil)
(defparameter *empty* nil)


(defun render-map (ctx level)
  (unless *solid*
    (setf *solid* (clgfw:make-color :r 100 :g 100 :b 100)))
  (unless *empty*
    (setf *empty* (clgfw:make-color :r 200 :g 200 :b 200)))
  (clgfw:draw-rectangle ctx 0 0
                        (clgfw:get-window-width ctx)
                        (clgfw:get-window-height ctx)
                        *solid*)
  
  (loop :with sz = (tilesize level)
        :for row :across (tilemap level)
        :for y :from 0 :by sz
        :do
           (loop :for tile :across row
                 :for x :from 0 :by sz
                 :when (= tile 1)
                   :do (clgfw:draw-rectangle ctx x y sz sz *empty*))))

(defun create-level ()
  (make-instance 'level
                 :map (create-map)
                 :entities (list (make-instance 'player :x 1 :y 1))))

(defun main ()
  (let* ((level (create-level)))
    (clgfw:with-window ctx (800 800 "Hello")
      (clgfw:while-running/with-drawing ctx
        (when (clgfw:is-key-down ctx :q)
          (return-from main))

        ;; Update
        (loop :for e :in (entities level)
              :do (update ctx level e))

        ;;Render
        (render-map ctx level)
        (loop :for e :in (entities level)
              :do (render ctx level e))

        )))
  )
