;; https://www.reddit.com/r/Common_Lisp/comments/1ey8f10/how_do_you_perform_a_noncontiguous_inplace/

;; I'm really proud of this for some reason.

;; > (compare-functions 10000000)
;; Original: 1279011/1000000 seconds
;; Original 2: 813007/1000000 seconds
;; New: 118601/100000 seconds
;; Super Optimized: 131501/500000 seconds

(defun new-select-sort (a)
  (declare (optimize (speed 3) (safety 0))) ; my first version had 0 compiler notes!
  (let ((indices '(0 2 4 6)))
    (let ((sublist (loop for i in indices collect (nth i a)))) ; make new list to destructively sort
      (setf sublist (sort sublist #'<))
      (loop for i in indices
            for val in sublist
            do (setf (nth i a) val)) ; put those sorted values in
      a)))

(defun original-select-sort (a)
 ; (declare (optimize (speed 3) (safety 0))) ; seems to slow it down, actually
  (let ((result (sort (list (elt a 0) (elt a 2) (elt a 4) (elt a 6)) #'<)))
    (setf (elt a 0) (elt result 0)
          (elt a 2) (elt result 1)
          (elt a 4) (elt result 2)
          (elt a 6) (elt result 3))
    a))

(defun original-select-sort-2 (a) ; the original, but following the compiler advice
  (declare (optimize (speed 3) (safety 0)))
  (declare (type list a))
    (let ((result (sort (vector (nth 0 a) (nth 2 a) (nth 4 a) (nth 6 a)) #'<)))
    (declare (type (simple-array t (4)) result))
    (setf (elt a 0) (elt result 0)
          (elt a 2) (elt result 1)
          (elt a 4) (elt result 2)
          (elt a 6) (elt result 3))
    a))

(defun super-optimized-select-sort (a)
  (declare (optimize (speed 3) (safety 0)))
  (let ((x0 (nth 0 a))
        (x2 (nth 2 a))
        (x4 (nth 4 a))
        (x6 (nth 6 a)))
    (declare (fixnum x0 x2 x4 x6)) ; this is obviously cheating
    (macrolet ((swap (a b)
                 `(when (< ,b ,a)
                    (rotatef ,a ,b))))
      (swap x0 x2)
      (swap x0 x4)
      (swap x0 x6)
      (swap x2 x4)
      (swap x2 x6)
      (swap x4 x6))
    (setf (nth 0 a) x0
          (nth 2 a) x2
          (nth 4 a) x4
          (nth 6 a) x6))
  a)

(defun benchmark (func iterations)
  (let ((start-time (get-internal-real-time)))
    (dotimes (i iterations)
      (let ((a (list 71 'a 32 'b 5 'c -8)))
        (funcall func a)))
    (/ (- (get-internal-real-time) start-time)
       internal-time-units-per-second)))

(defun compare-functions (iterations)
  (format t "Original: ~A seconds~%"
          (benchmark #'original-select-sort iterations))
  (format t "Original 2: ~A seconds~%"
          (benchmark #'original-select-sort-2 iterations))
  (format t "New: ~A seconds~%"
          (benchmark #'new-select-sort iterations))
  (format t "Optimized: ~A seconds~%"
      (benchmark #'super-optimized-select-sort iterations)))
