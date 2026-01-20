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
  (when (clgfw::is-key-down ctx :j)
    (incf (y player))
    )
  )
(defmethod render (ctx level (player player))
  (clgfw:draw-rectangle
   ctx
   (x player)
   (y player)
   (tilesize level)
   (tilesize level)
   (clgfw:make-color :r 200)))

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

(defun render-map (ctx level)
  (loop :with solid = (clgfw:make-color :r 100 :g 100 :b 100)
        :with empty = (clgfw:make-color :r 200 :g 200 :b 200)
        :with sz = (tilesize level)
        :for row :across (tilemap level)
        :for y :from 0 :by sz
        :do
           (loop :for tile :across row
                 :for x :from 0 :by sz
                 :for color = (if (= tile 1) solid empty)
                 :do (clgfw:draw-rectangle ctx x y sz sz color))))

(defun create-level ()
  (make-instance 'level
                 :map (create-map)
                 :entities (list (make-instance 'player))))

(defun main ()
  (let* ((level (create-level)))
    (clgfw:with-window ctx (800 800 "Hello")
      (clgfw:while-running/with-drawing ctx

        ;; Update
        (loop :for e :in (entities level)
              :do (update ctx level e))

        ;;Render
        (render-map ctx level)
        (loop :for e :in (entities level)
              :do (render ctx level e))

        )))
  )
