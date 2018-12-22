;;;===========================================================================
;;; OM-ModTile - tiling modulo 2 functions
;;; 
;;; Hélianthe Caure (IRCAM - 2016)
;;;===========================================================================

(in-package :om)

;;;=============================
;;; API
;;;=============================
(defmethod! find_rhythm_mod_2 (A) 
            :icon 3
            :initvals '((0 1 3))
            :doc "Find entry patterns to tile mod 2."
            :indoc '("A rhythm")
            (find_out_rhythm A))

(defmethod! make_mod_2_canon (A B) 
            :icon 3
            :initvals '((0 1 3) (0 2 3))
            :doc "Outputs the mod 2 canon as a chord sequence."
            :indoc '("Inner rhythm" "Outer rhythm")
            (let ((pulse 250))
              (make-instance 'multi-seq
                             :chord-seqs 
                             (reverse (loop for item in B
                                   for pitch = 6000 then (+ pitch 400)
                                   collect 
                                   (let ((onsets (om* pulse (om+ item A))))
                                     (make-instance 'chord-seq
                                                  :Lmidic (make-list (length onsets) :initial-element pitch)
                                                  :Lonset onsets)))))))

(defmethod! is_vuza_p (A)
            :icon 3
            :initvals '((0 1 3) )
            :doc "Checks if a rythm can give a mod 2 Vuza canon."
            :indoc '("Inner rhythm")
            (let ((B (find_out_rhythm A)))
              (equal A (find_out_rhythm B))))

(defmethod! zoom-canon ((A list) (k integer))
            :numouts 2
            :icon 3
            :initvals '((0 1 2 3 4) 2)
            :doc "From a mod 2 canon, creates 2 mod 2 canons that are k times longer than the original, and that are different but have the same donsets."
            :indoc '("A rhythm" "An integer")
            (let* ((B (find_out_rhythm A))
                   (a1 (om* A k))
                   (b2 (om* B k))
                   a2 b1)
              (setf b1 (mult_et_somme_directe B k))
              (setf a2 (mult_et_somme_directe A k))
              (values (list a1 b1) (list a2 b2))))

(defmethod! is_transferable_p (A)
            :icon 3
            :doc "Gives the integers p that can produce a p-transfer."
            :indoc '("A rhythm")
  (let* ((B (find_out_rhythm A))
         (period (+ 1 (car (last A)) (car (last B))))
         (primes (loop for item in (factorize period) collect (car item))))
    (loop for item in primes
          when (and (is_transferable_t1_p A (/ period item) item) (is_transferable_t1_p B (/ period item) item))
          collect item)))

(defmethod! transfert-transform (A k)
            :numouts 2
            :icon 3
            :initvals '((0 1 2 3 4) 2)
            :doc "From a mod 2 canon, creates 2 mod 2 canons that are 2 times longer than the original with the k-transfer transformation, and that are different but have the same donsets."
            :indoc '("A rhythm" "An integer")
            (let ((rep (is_transferable_p A)))
              (cond
               ((null rep) (print "No possible p-transfer!"))
               ((not (member k rep)) (format nil "K must be in ~A" rep))
                (t (let* ((B (find_out_rhythm A))
                          (period (+ 1 (car (last A)) (car (last B))))
                          (n (/ period k))
                          (a1 (remove-k-pairs (somme_directe A n k) k))
                          (b2 (remove-k-pairs (somme_directe B n k) k))
                          (b1 (remove-k-pairs (somme_directe B 2 k) k))
                          (a2 (remove-k-pairs (somme_directe A 2 k) k)))
                     (values (list a1 b1) (list a2 b2)))))))


;;;=============================
;;; Hidden tools
;;;=============================
(defmethod find_out_rhythm (A)
  (let* ((b '(0)) 
         (patt (make_memo A))
         (memo patt))
    (loop while (has_zero_memo? memo) do
          (setf memo (superpose_memo memo patt))
          (setf b (append b (list (find-first-zero memo))))
          (setf memo (simplify_memo memo)))
    (setf b (remove nil (cddr b)))
    (dx->x 0 b)))

(defun make_memo (r)
  (let ((rep (make-list (+ (car (last r)) 1) :initial-element 0)))
    (loop for item in r do
          (setf (nth item rep) 1))
    rep))

(defun find-first-zero (memo)
  (position 0 memo))

(defun has_zero_memo? (memo)
  (member 0 memo))

(defun simplify_memo (memo)
  (member 0 memo))

(defun superpose_memo (memo1 memo2)
  (let* ((l1 (length memo1))
         (l2 (length memo2))
         (suffix (make-list (- l2 l1) :initial-element 0)))
    (mod+ memo2 (append memo1 suffix) 2)))

(defun somme_directe (A n k)
 (sort (loop for item in A append
        (loop for i from 0 to (- n 1) collect
              (+ (* k i) item))) '<))

(defun remove-k-pairs (l k)
  (cond
   ((null l) nil)
   ((not (= 0 (mod (car l) k))) (cons (car l) (remove-k-pairs (cdr l) k)))
   (t (let ((liste (remove (car l) l)))
       (if (= 0 (mod (- (length l) (length liste)) 2))
           (remove-k-pairs liste k)
         (cons (car l) (remove-k-pairs liste k)))))))

(defun mult_et_somme_directe (A k)
  (loop for item in A append
        (loop for i from 0 to (- k 1) collect
              (+ (* k item) i))))

(defun is_transferable_t1_p (A n k)
  (let* ((somme (somme_directe A n k))
         (rep (remove-duplicates (get-duplicates somme))))
    (or (null rep) (all-k-factors rep k))))

(defun get-duplicates (l)
  (cond
   ((null l) nil)
   (t (if (member (car l) (cdr l))
          (cons (car l) (get-duplicates (cdr l)))
        (get-duplicates (cdr l))))))

(defun all-k-factors (l k)
  (let ((rep t))
    (loop for item in l
          while rep do
          (unless (= (mod item k) 0)
            (setf rep nil)))
    rep))