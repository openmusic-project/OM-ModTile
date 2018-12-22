;;;===========================================================================
;;; OM-ModTile - automaton-tiling editor
;;; 
;;; Hélianthe Caure (IRCAM - 2016)
;;;===========================================================================

(in-package :om)

;;;Editor
(defclass automaton-tiling-editor (editorview object-editor) 
  ((tri-color :initform (om-make-color (/ 158 256.0) (/ 82 256.0) (/ 235 256.0)) :accessor tri-color)
   (tri-size :initform 2.5 :accessor tri-size)
   (zoom :initform 1.0 :accessor zoom)))

;;;
(defmethod set-tri-color ((self automaton-tiling-editor))
  (setf (tri-color self) 
               (om-choose-color-dialog :color (tri-color self)))
  (om-invalidate-view self))

;;;
(defmethod zoom+ ((self automaton-tiling-editor))
  (incf (zoom self) 
               0.1)
  (om-invalidate-view self))

;;;
(defmethod zoom- ((self automaton-tiling-editor))
  (setf (zoom self) 
        (max 0.1 (- (zoom self) 0.1)))
  (om-invalidate-view self))

;;;
(defmethod point+ ((self automaton-tiling-editor))
  (incf (tri-size self) 0.1)
  (om-invalidate-view self))

;;;
(defmethod point- ((self automaton-tiling-editor))
  (setf (tri-size self) 
        (max (- (tri-size self) 0.1) 0.1))
  (om-invalidate-view self))


;;;Panel (view)
(defclass automaton-tiling-panel (om-view) 
  ())

;;;Transport (play/stop)
(defclass automaton-tiling-transport (om-view) 
  ())

;;;Key Handler (Space = play/pause ; S = reset)
(defmethod handle-key-event ((Self automaton-tiling-panel) Char)
  (let ((obj (object (om-view-container self))))
    (case char
      (#\space
       (if (eq (state obj) :stop)
           (play-automaton obj)
         (stop-automaton obj)))
      (#\s
       (reset-automaton obj))
      (t nil))))


;;;Build editor window
(defmethod make-editor-window ((class (eql 'automaton-tiling-editor)) object name ref &key 
                               winsize winpos (close-p t) (winshow t) 
                               (resize nil) (maximize nil))
  (let ((win (call-next-method class object name ref :resize t
                               :winsize (om-make-point 100 100) :winpos winpos :resize nil 
                               :close-p t :winshow t :bg-color *om-dark-gray-color*)))
    win))

;;;Close editor window
(defmethod close-editor-before ((self automaton-tiling-editor))
  (stop-automaton (object self)))

;;;Initialize editor window (remove titlebar, add panel view)
(defmethod initialize-instance :after ((Self automaton-tiling-editor) &rest l) 
  (declare (ignore l))
  (setf (panel self) (om-make-view 'automaton-tiling-panel
                                   :scrollbars t
                                   :retain-scrollbars nil
                                   :bg-color (om-make-color (/ 255 256.0) (/ 255 256.0) (/ 234 256.0))
                                   :field-size  (om-make-point (* (1+ (n (object self))) 10) 1000000)
                                   :size (om-make-point (w self) (h self))
                                   :position (om-make-point 0 20)))
  (setf (display (object self)) (panel self))
  (om-remove-subviews self (car (om-subviews self)))
  (om-add-subviews self (om-make-view 'automaton-tiling-transport
                                      :position (om-make-point 0 0)
                                      :bg-color *om-dark-gray-color*
                                      :size (om-make-point (w self) 20)))
  (om-add-subviews self (panel self)))

;;;Resize callback
(defmethod update-subviews ((self automaton-tiling-editor))
  (om-set-view-size (car (om-subviews self)) (om-make-point (w self) (h self)))
  (om-set-view-size (panel self) (om-make-point (w self) (h self))))

;;;Draw panel content
(defmethod om-draw-contents ((self automaton-tiling-panel))
  (let* ((obj (object (om-view-container self)))
         (f (zoom (om-view-container self)))
         (s (* f (tri-size (om-view-container self))))
         (xo 10)
         (yo 0))
    (om-with-focused-view self
      (loop for pt in (word obj)
            for i from 0
            ;mettre condition si caché 
            do
            (if (= (mod i (n obj)) 0)
                (incf yo (* f 10)))
            (if (= pt 0)
                (om-fill-ellipse (+ xo (* f (mod i (n obj)) 10)) yo f f)
              (if (= pt 1)
                  (om-fill-ellipse (+ xo (* f (mod i (n obj)) 10)) yo (* 2 f) (* 2 f))
                (om-with-fg-color self (tri-color (om-view-container self))
                  (om-fill-ellipse (+ xo (* f (mod i (n obj)) 10)) yo s s))))))))

;;;Draw transport content
(defmethod om-draw-contents ((self automaton-tiling-transport))
    (om-with-focused-view self
      (om-with-fg-color self *om-white-color*
        (om-draw-rect 3 3 40 14)
        (om-draw-string 11 15 "PLAY")
        (om-draw-rect 48 3 40 14)
        (om-draw-string 54 15 "PAUSE")
        (om-draw-rect 93 3 40 14)
        (om-draw-string 99 15 "RESET")
        (om-draw-rect 138 3 40 14)
        (om-draw-string 141 15 "EXPORT")
        (om-draw-rect 183 3 40 14)
        (om-draw-string 188 15 "COLOR")
        (om-draw-rect 228 3 20 14)
        (om-draw-string 233 15 "p+")
        (om-draw-rect 253 3 20 14)
        (om-draw-string 258 15 "p-")
        (om-draw-rect 278 3 20 14)
        (om-draw-string 283 15 "z+")
        (om-draw-rect 303 3 20 14)
        (om-draw-string 308 15 "z-"))))

;;;handle transport clicks
(defmethod om-view-click-handler ((self automaton-tiling-transport) where)
  (cond ((and (>= (om-point-x where) 3)
              (< (om-point-x where) 47))
         (play-automaton (object (om-view-container self))))
        ((and (>= (om-point-x where) 49)
              (< (om-point-x where) 90))
         (stop-automaton (object (om-view-container self))))
        ((and (>= (om-point-x where) 93)
              (< (om-point-x where) 134))
         (reset-automaton (object (om-view-container self))))
        ((and (>= (om-point-x where) 138)
              (< (om-point-x where) 178))
         (export-as-picture (object (om-view-container self))))
        ((and (>= (om-point-x where) 183)
              (< (om-point-x where) 223))
         (set-tri-color (om-view-container self)))
        ((and (>= (om-point-x where) 227)
              (< (om-point-x where) 249))
         (point+ (om-view-container self)))
        ((and (>= (om-point-x where) 252)
              (< (om-point-x where) 273))
         (point- (om-view-container self)))
        ((and (>= (om-point-x where) 277)
              (< (om-point-x where) 298))
         (zoom+ (om-view-container self)))
        ((and (>= (om-point-x where) 302)
              (< (om-point-x where) 323))
         (zoom- (om-view-container self)))))

;;;Draw function used in export
(defmethod draw-image ((self automaton-tiling-panel))
  (let ((obj (object (om-view-container self)))
        (s (tri-size (om-view-container self)))
        (xo 10)
        (yo 0))
    (loop for pt in (word obj)
          for i from 0
          do
          (if (= (mod i (n obj)) 0)
              (incf yo 10))
          (if (= pt 0)
              (om-fill-ellipse (+ xo (* (mod i (n obj)) 10)) yo 1 1)
            (if (= pt 1)
                (om-fill-ellipse (+ xo (* (mod i (n obj)) 10)) yo 2 2)
              (om-with-fg-color om-api::*curstream* (tri-color (om-view-container self))
                (om-fill-ellipse (+ xo (* (mod i (n obj)) 10)) yo s s)))))))