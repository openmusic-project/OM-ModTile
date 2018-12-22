;;;===========================================================================
;;; OM-ModTile - automaton-tiling object
;;; 
;;; Hélianthe Caure (IRCAM - 2016)
;;;===========================================================================

(in-package :om)

;;;======================
;;; Object
;;;======================
(defclass! automaton-tiling (named-object)
  (;;;User
   (Aset :initform (list 0 1 4) :initarg :Aset :accessor Aset)
   (n :initform 4 :initarg :n :accessor n)
   ;;;Visible
   (word :initform (list 1) :accessor word)
   ;;;Hidden
   (Amax :initform 4 :accessor Amax)
   (cover :initform (list) :accessor cover)
   (cover-index :initform 0 :accessor cover-index)
   ;;;View to refresh
   (display :initform nil :accessor display)
   ;;;Player
   (process :initform nil :accessor process)
   (state :initform :stop :accessor state))
  (:icon 1)
  (:documentation "An object which editor automatically displays similarities in the entry pattern.
Play starts (or continues) the computation of the pattern.
Pause pauses the computation.
Reset does what it does.
Export can be used to save the automaton display as a PNG file. If n-rows is not set, picture size will be the same as the editor."))

;;;======================
;;; Export method
;;;======================
;;;Export automaton content as a picture
(defmethod! export-as-picture ((automaton automaton-tiling))
            :icon 2
            :doc "Export an automaton-tiling display as a PNG. 
Asks for a valid path and for the number of exported rows. 
If n-rows is not set, picture size will be the same as the editor."
            (if (display automaton)
                (let ((path (om-choose-new-file-dialog :prompt "Location to save picture..."
                                                          :name "automaton"
                                                          :types (list (format nil (om-str :file-format) "PNG") "*.png")))
                      rows)
                  (when path
                    (setq path (namestring path)
                          rows (om-get-user-string "How many rows do you want to export?"))
                    (if rows (setq rows (om-with-error-handle (parse-integer rows))))
                    (let ((pict (om-record-pict nil (if rows
                                                        (om-make-point (* (1+ (n automaton)) 10) 
                                                                       (+ 5 (* rows 10)))
                                                      (om-field-size (display automaton)))
                                  (draw-image (display automaton)))))
                      (om-save-picture pict path :png))))
              (print "Please open the automaton editor before exporting it.")))




;;;======================
;;; Hidden methods
;;;======================
;;;Basic redefinitions
(defmethod Class-has-editor-p ((self automaton-tiling)) t)
(defmethod get-initval ((self automaton-tiling))
  (make-instance (class-of self)))
(defmethod default-obj-box-size ((self automaton-tiling)) (om-make-point 80 60))
(defmethod get-editor-class ((self automaton-tiling)) 'automaton-tiling-editor)

;;;Initialization (prepare max(Aset) and cover)
(defmethod initialize-instance ((self automaton-tiling) &rest initargs)
  (call-next-method)
  (init-automaton-tiling self))
(defmethod init-automaton-tiling ((self automaton-tiling))
  (setf (Amax self) (reduce 'max (Aset self)))
  (setf (cover self) (append (cover self) (make-list (- (+ 1 (Amax self) (cover-index self)) (length (cover self)))
                                                     :initial-element 0))))

;;;Start the word computation process
(defmethod play-automaton ((self automaton-tiling))
  (setf (process self)
        (mp:process-run-function (format nil "~A" self) nil
                                 #'(lambda ()
                                     (let ((first-lines-done nil) i-end)
                                       ;;;start
                                       (setf (state self) :play)
                                       ;;;build the word step by step
                                       (loop while (setf (cover-index self) (position 0 (cover self)))
                                             do
                                             (setf (cover self) (append (cover self) (make-list (- (+ 1 (Amax self) (cover-index self)) (length (cover self)))
                                                                                                :initial-element 0)))
                                             (loop for i in (Aset self)
                                                   do
                                                   (setf (nth (+ i (cover-index self)) (cover self)) 
                                                         (mod (1+ (nth (+ i (cover-index self)) (cover self))) 2)))
                                             (setf (word self) (append (word self) (make-list (- (cover-index self) (1- (length (word self))))
                                                                                              :initial-element 0)))
                                             (setf (nth (1- (length (word self))) (word self)) 1)
                                             ;;;update graphics and check if not stopped
                                             (setq i-end (1- (length (word self))))

                                             ;;;Mark triangles
                                             ;First two lines
                                             (if (and (not first-lines-done)
                                                      (> (length (word self)) (* 3 (n self))))
                                                 (progn
                                                   ;;;ligne1
                                                   (dotimes (j (n self))
                                                     (if (and (plusp (nth j (word self)))
                                                              (plusp (nth (+ j (n self)) (word self))))
                                                         (setf (nth j (word self)) 2)))
                                                   ;;;ligne2
                                                   (loop for j from (n self) to (1- (* 2 (n self)))
                                                         do
                                                         (if (and (plusp (nth j (word self)))
                                                                  (plusp (nth (+ (1- j) (* 2 (mod (Amax self) 2))) (word self))))
                                                             (progn
                                                               (setf (nth j (word self)) 2)
                                                               (if (= (nth (+ (- j 2) (* 4 (mod (Amax self) 2))) (word self)) 0)
                                                                   (setf (nth (+ (1- j) (* 2 (mod (Amax self) 2))) (word self)) 2)))))
                                                   (setq first-lines-done t)))
                                             ;Rest
                                             (if (> (length (word self)) (* 2 (n self)))
                                                 (progn
                                                   (if (plusp (nth (- i-end (n self)) (word self)))
                                                       (progn
                                                         (setf (nth i-end (word self)) 2)
                                                         (if (= (nth (- i-end (* 2 (n self))) (word self)) 0)
                                                             (setf (nth (- i-end (n self)) (word self)) 2))))
                                                   (if (< (+ (1- i-end) (* 2 (mod (Amax self) 2))) (length (word self)))
                                                       (if (plusp (nth (+ (1- i-end) (* 2 (mod (Amax self) 2))) (word self)))
                                                           (progn
                                                             (setf (nth i-end (word self)) 2)
                                                             (if (= (nth (+ (- i-end 2) (* 4 (mod (Amax self) 2))) (word self)) 0)
                                                                 (setf (nth (+ (1- i-end) (* 2 (mod (Amax self) 2))) (word self)) 2))))
                                                     (if (and (= (mod (Amax self) 2) 1) 
                                                              (plusp (nth (1- i-end) (word self))))
                                                         (setf (nth i-end (word self)) 2)))))
                                             (refresh-automaton self))
                                       ;;;Add missing 0 in the word
                                       (setf (word self) (append (word self) (make-list (- (n self) (mod (length (word self)) (n self))) :initial-element 0)))
                                       ;;;stop
                                       (refresh-automaton self)
                                       (setf (state self) :stop)
                                       (print "...Automaton computation ended")))))
  (print "Automaton computation just started..."))

;;;Stop the computation process
(defmethod stop-automaton ((self automaton-tiling)) (setf (state self) :stop) (print "......Automaton computation paused"))

;;;Refresh automaton and check its state
(defmethod refresh-automaton ((self automaton-tiling))
  (if (display self)
      (om-invalidate-view (display self)))
  (if (and (process self)
           (eq (state self) :stop))
      (progn
        (mp:process-kill (process self))
        (setf (process self) nil))))

;;;Reset automaton
(defmethod reset-automaton ((self automaton-tiling))
  (if (eq (state self) :stop)
      (progn 
        (setf (word self) '(1)
              (cover self) '()
              (cover-index self) 0)
        (init-automaton-tiling self)
        (refresh-automaton self))
    (print "Please pause automaton to re-init")))


 


;;;mark triangles after the whole word is computed
;;;NOT USED because performed dynamically while computing the word
(defmethod mark-automaton-triangles ((self automaton-tiling) i-start i-end)
  (loop for i from i-start to i-end
        do
        (when (and (> (nth i (word self)) 0)
                   (plusp (loop for i-voisins in (if (> i (n self))
                                                     (list (- i (n self)) (- (1+ i) (* 2 (mod (Amax self) 2))) (+ i (n self)))
                                                   (list (+ i (n self))))
                                sum
                                (or (nth i-voisins (word self)) 0))))
          (setf (nth i (word self)) 2)
          (refresh-automaton self))))