;; * lists-to-xml

(in-package :sc)

;; *** nth-mod
(defun nth-mod (n rthm-ls)
  (nth (mod n (length rthm-ls)) rthm-ls)) 

;; *** split-into-simpler-ratios
;;; returns a list
;;; very simple, could be imprived.
(defun split-into-simpler-ratios (num)
  (setf num (rationalize num))
  (if (and (> (numerator num) 1) (= 0 (mod (denominator num) 2)))
      (let* ((simpler (/ (floor (numerator num) 2) (/ (denominator num) 2))))
	(append (split-into-simpler-ratios simpler)
		(split-into-simpler-ratios (- num simpler))))
      (list num)))

;; *** quantise-for-notation
;;; quantising to some hopefully notatable durations
;;; returns a list of durations which add up to a quantised version of the
;;; original duration. This is a very simple approach, could be improved.
;;; This basically uses the #'nearest function to quantise to a value that
;;; is a normal note, dotted note or tuplet (3 and 5).
(let* ((steps
	 (append 
	  (loop for i from 1 to 6 collect (/ 1 (expt 2 i)))
	  (loop for i from 0 to 3 collect (/ 1 (* 3 (expt 2 i))))
	  (loop for i from 0 to 2 collect (/ (* 5 (expt 2 i))))))
       (possible-divisions
	 (sort
	  (remove-duplicates
	   (loop for i in steps
		 append (loop for time from 0 to 1 by i collect time)))
	  #'<)))
  ;; defun...
  (defun quantise-for-notation (duration &key (tempo 60))
    (unless (> duration 0)
      (error "duration should be greater than 0 but is: ~a" duration))
    (let* ((tempo-mult (/ tempo 240))
	   ;; normalise to tempo = 240 (1 second per 4/4 bar)
	   (normalised-duration (* duration tempo-mult))
	   (full-bars 0)
	   (quantised 0))
      (loop while (> normalised-duration 1)
	    do (incf full-bars)
	       (incf normalised-duration -1))
      (setf quantised (nearest normalised-duration possible-divisions))
      (loop for i in (append (ml 1 full-bars)
			     (split-into-simpler-ratios quantised))
	    collect (/ i tempo-mult)))))

;; *** split-durations-for-bars
;;; returns a list of sublists of durations. These durations can be fit into
;;; bars at tempo but the sublists mark how the original durations belong
;;; together.
(defun split-durations-for-bars (duration-list
				  &optional (time-sig '(4 4)) (tempo 60))
  (let* ((result '())
	 ;; duration of a bar in seconds:
	 (duration-of-bar (* 60 (/ 4 (cadr time-sig) tempo) (car time-sig))))
    (setf duration-of-bar (rationalize duration-of-bar))
    (loop while duration-list
	  with current-sum = 0
	  with was-split
	  for next-dur = (pop duration-list)
	  for next-sum = (+ current-sum next-dur)
	  do (cond ((equal-within-tolerance next-sum duration-of-bar 0.001)
		    (if was-split
			(push (append (pop result) `(,next-dur)) result)
			(push `(,next-dur) result))
		    (setf current-sum 0 was-split nil))
		   ((< next-sum duration-of-bar)
		    (if was-split
			(push (append (pop result) `(,next-dur)) result)
			(push `(,next-dur) result))
		    (setf current-sum next-sum was-split nil))
		   ((> next-sum duration-of-bar)
		    (let ((diff (rationalize (- duration-of-bar current-sum))))
		      (if was-split
			  (push (append (pop result) `(,diff)) result)
			  (push `(,diff) result))
		      (push (rationalize (- next-dur diff)) duration-list)
		      (setf current-sum 0 was-split t)))))
    (reverse result)))

;; *** quantise-sublists-for-notation
;;; quantise-for-notation when already split for bars
(defun quantise-sublists-for-notation (list-of-split-durations &key (tempo 60))
  (loop for bar in list-of-split-durations
	collect (loop for dur in bar
		      append (quantise-for-notation dur :tempo tempo))))

;; *** durations-and-pitches-to-events
;;; Arguments:
;;; - a list of durations in seconds.
;;; - a list of pitches, where pitches in a sublist are a chord and nil is
;;; a rest: '(e5 (c5 g5) nil). 
;;; The number of notes to be notated is always the number of elements in
;;; the duration-list. If pitch-list is short, it is loooped.
;;; Returns:
;;; a list of events that are split, so they fit in a bar with the given time 
;;; signature in the given tempo.
;;; Because events might have to be split into several events that are tied to
;;; each other, the original index of a pitch might not be the same index for
;;; this pitch in the resulting event list. Thus a second return value is a list
;;; with all indices of the original elements in the pitch-list (sublists count
;;; as one).
(defun durations-and-pitches-to-events
    (duration-list pitch-list &optional (time-sig '(4 4)) (tempo 60))
  (unless (>= (length pitch-list) (length duration-list))
    (setf pitch-list
	  (loop for i from 0 below (length duration-list)
		collect (nth-mod i pitch-list))))
  (setf duration-list 
	(quantise-sublists-for-notation
	 (split-durations-for-bars duration-list time-sig tempo)))
  (let* ((diff 0)
	 (result '())
	 (indices-of-attacks '())
	 (duration-of-bar (* 60 (/ 4 (cadr time-sig) tempo) (car time-sig))))
    (setf indices-of-attacks
	  (loop for tied in duration-list with i = 0
		collect i do (incf i (length tied))))
    (setf diff (- duration-of-bar
		  (mod (apply #'+ (flatten duration-list)) duration-of-bar)))
    (when (= diff duration-of-bar) (setf diff 0))
    (setf result
	  (loop for tied in duration-list and pitch in pitch-list
		append (loop for dur in tied and k from 1 with len = (length tied)
			     for is-rest = (not pitch)
			     for tied-from = (unless is-rest (< k len))
			     for tied-to = (unless is-rest (> k 0))
			     collect (make-event pitch dur
						 :duration t :tempo tempo
						 :is-tied-from tied-from
						 :is-tied-to tied-to
						 :is-rest is-rest))))
    (unless (equal-within-tolerance diff 0 .001)
      (let ((qdiff (quantise-for-notation diff :tempo tempo)))
	(setf result
	      (append result
		      (loop for diff in qdiff
			    collect (make-event nil diff :is-rest t :duration t
							 :tempo tempo))))))
    (loop for e in result
	  do (when (= -1 (letter-value e))
	       (setf (letter-value e)
		     (floor (/ 4 (* (duration e))))))) ; ? crude aprox
    (values result indices-of-attacks)))

;; *** lists-to-xml
;;; Arguments:
;;; -lists should be a list of lists. These sublists should have this format:
;;; '(player instrument (list-of-durations) (list-of-pitches)
;;;   ((index mark1) (index mark2)))
;;; the list of marks is optional. The index refers to the index of the pitch in
;;; the pitch list at which to put the mark. Marks can be any slippery chicken
;;; mark - for example just a string with text.
;;; Note, that if one of duration or pitch list is shorter than the other, the
;;; shorter one will be looped until it has the same length as the longer one.
;;; -file should be the path to and the filename for the resulting .xml file.
;;;  If file is nil, one will be automatically generated from the load-path of
;;;  this file.
;;;
;;; This is essentially an implementation of this:
;;; https://github.com/mdedwards/slippery-chicken/wiki/How-can-I-'roll-my-own'-slippery-chicken%3F
;;;
;;; EXAMPLES
#|
;; simple example using one instrument:
(lists-to-xml '((player-one piano (4 4 6 2) (e5 nil d5 a5) ((0 "schneller") (2 "pp")))) 
	      nil :tempo 120)

;; same but change time signature:
(lists-to-xml '((player-one piano (4 4 6 2) (e5 nil d5 a5) ((0 "schneller") (2 "pp"))))
	      nil :tempo 120 :time-sig '(3 4))

;; more interesting time-sig:
(lists-to-xml '((player-two flute (6) ((e5 a5)) ((0 "schneller") (2 "pp"))))
	      nil :time-sig '(5 8))

;; complex example, which will produce a corrupted file - because I would rathe
;; have it produce corrupt notation than fail:
(lists-to-xml '((player-one piano (3 3 3.3 4.83 3) (e5 nil d5 a5) ((0 "schneller") (2 "pp")))
		(player-two flute (6) ((e5 a5)) ((0 "schneller") (2 "pp")))
		(player-three bassoon (2.6) (e5) ((0 "schneller") (2 "pp"))))
	      nil :tempo 70 :time-sig '(5 8))
|#
(let ((default-dir (path-from-same-dir)))
  (defun lists-to-xml (lists file &key (time-sig '(4 4)) (tempo 60)
				    title composer)
    ;; maybe set file and title
    (unless title
      (setf title (if file (pathname-name file) "untitled")))
    (unless file
      (setf file (format nil "~a~a~a~a"
			 default-dir "lists-to-xml-" title ".xml")))
    ;; sanity checks:
    (unless (and (listp lists) (loop for ls in lists always (listp ls)))
      (error "lists in lists-to-xml seems to be malformed"))
    (let* (sc list-of-list-of-bars)
      (loop for i in lists
	    ;; split durations so they fit into bars and parse to (tied) events
	    do (multiple-value-bind (events attacks-indices)
		   (durations-and-pitches-to-events
		    ;; instead of giving the tempo argument to
		    ;; #'durations-and-pitches-to-events I normalize to
		    ;; 60 because events do that anyways.
		    (loop for k in (third i) collect (* k (/ tempo 60)))
		    (fourth i) time-sig)
		 ;; add marks to events (using a list of indices of the events
		 ;; before they were split into bars).
		 (loop for m in (fifth i) 
		       when (< (car m) (length attacks-indices))
			 do (add-mark (nth (nth (car m) attacks-indices) events)
				(cadr m)))
		 ;; generate bars
		 (push (loop while events
			     for bar = (make-rthm-seq-bar `(,time-sig))
			     for ate = (fill-with-rhythms bar events)
			     do (setf events (when ate (nthcdr ate events)))
			     collect bar)
		       list-of-list-of-bars)))
      ;; pad list-of-bars that are shorter than others in list-of-list-of-bars
      (let ((list-of-bars-len (mapcar #'length list-of-list-of-bars)))
	(unless (apply #'= list-of-bars-len)
	  (let* ((max-len (apply #'max list-of-bars-len))
		 (empty-bar (make-rthm-seq-bar `(,time-sig))))
	    (fill-with-rhythms
	     empty-bar `(,(make-event nil (apply #'/ time-sig)
				      :duration t :is-rest t)))
	    (setf list-of-list-of-bars
		  (loop for bars in list-of-list-of-bars
			collect (append bars
					(ml empty-bar
					    (- max-len (length bars)))))))))
      ;; put them in a slippery-chicken object:
      (loop for bars in (reverse list-of-list-of-bars) and i in lists
	    do (setf sc (bars-to-sc bars :sc sc :player (first i)
					 :instrument (second i) :tempo tempo)))
      ;; set composer and title
      (setf (composer sc) composer (title sc) title)
      (check-ties sc t)
      ;; call write-xml on sc-object
      (write-xml sc :file file))))

;; EOF lists-to-xml.lsp
