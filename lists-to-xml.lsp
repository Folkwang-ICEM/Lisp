;; * lists-to-xml

(in-package :sc)

;; *** nth-mod
(defun nth-mod (n rthm-ls)
  (nth (mod n (length rthm-ls)) rthm-ls))

;; *** pad-lists
;;; if one list is shorter than the other, loop it until is has the same length
(defun pad-lists (ls1 ls2)
  (if (> (length ls1) (length ls2))
      (setf ls2 (loop for i from 0 below (length ls1)
		      collect (nth-mod i ls2)))
      (setf ls1 (loop for i from 0 below (length ls2)
		      collect (nth-mod i ls1))))
  (values ls1 ls2))

;; *** durations-and-pitches-to-events
;;; Arguments: a list of durations in seconds and of pitches, where pitches in
;;; a sublist are a chord and nil is a rest: '(e5 (c5 g5) nil). If one list is
;;; shorter than the other, the shorter one is looped so both have the same
;;; length.
;;; Returns a list of events that are split, so they fit in a bar with the given
;;; time signature in the given tempo.
;;; Because events might have to be split into several events that are tied to
;;; each other, the original index of a pitch might not be the same index for
;;; this pitch in the resulting event list. Thus a second return value is a list
;;; with all indices of the original elements in the pitch-list.
(defun durations-and-pitches-to-events (duration-list pitch-list
					&optional (time-sig '(4 4)) (tempo 60))
  ;; loop the shorter list to get the same length for both:
  (multiple-value-bind (ls1 ls2) (pad-lists duration-list pitch-list)
    (setf duration-list ls1 pitch-list ls2))
  (let* ((events '())
	 (original-events-indices '())
	 ;; duration of a bar in seconds:
	 (duration-of-bar (rationalize (* 60 (/ 4 (cadr time-sig) tempo)
					  (car time-sig)))))
    (loop while (and duration-list pitch-list)
	  with is-tied
	  with current-sum = 0
	  for i from 0
	  for is-rest = (not (car pitch-list)) ;; nil means rest
	  for next-dur = (rationalize (pop duration-list))
	  for next-sum = (+ current-sum next-dur)
	  do (unless is-tied (push i original-events-indices))
	     ;; if bar is full now
	     (cond ((equal-within-tolerance next-sum duration-of-bar 0.001)
		    (push (make-event (pop pitch-list) next-dur
				      :is-tied-to is-tied :duration t
				      :is-rest is-rest :tempo tempo)
			  events)
		    (setf current-sum 0 is-tied nil))
		   ;; if bar is not full now
		   ((< next-sum duration-of-bar)
		    (push (make-event (pop pitch-list) next-dur
				      :is-tied-to is-tied :duration t
				      :is-rest is-rest :tempo tempo)
			  events)
		    (setf current-sum next-sum is-tied nil))
		   ;; if duration is too long for this bar
		   ((> next-sum duration-of-bar)
		    (let ((diff (- duration-of-bar current-sum)))
		      (push (make-event (car pitch-list) diff
					:is-tied-to is-tied :is-tied-from t
					:duration t :is-rest is-rest
					:tempo tempo)
			    events)
		      (push (- next-dur diff) duration-list))
		    (setf current-sum 0 is-tied (unless is-rest t))))
	     ;; fill last bar with rests until full
	  finally (unless (= 0 current-sum)
		    (push (make-event nil (- duration-of-bar current-sum)
				      :duration t :is-rest t :tempo tempo)
			  events)))
    (loop for e in events
	  do (when (= -1 (undotted-value e))
	       (setf (undotted-value e) 1)))
    ;; return events and original indices
    (values (reverse events)
	    (reverse original-events-indices))))

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
;;; -file should be the path to and the filename for the resulting .xml file
;;;
;;; This is essentially an implementation of this:
;;; https://github.com/mdedwards/slippery-chicken/wiki/How-can-I-'roll-my-own'-slippery-chicken%3F
;;;
;;; EXAMPLES
#|
;; simple example using one instrument:
(lists-to-xml '((player-one piano (4 4 6 2) (e5 nil d5 a5) ((0 "schneller") (2 "pp")))) 
	      "/E/code/ensemble/test.xml" :tempo 120)

;; same but change time signature:
(lists-to-xml '((player-one piano (4 4 6 2 1) (e5 nil d5 a5) ((0 "schneller") (2 "pp"))))
	      "/E/code/ensemble/test.xml" :tempo 120 :time-sig '(3 4))

;; complex example:
(lists-to-xml '((player-one piano (3 3 3 4 3) (e5) )
		(player-two flute (6) ((e5 a5)) ((0 "schneller") (2 "pp")))
		(player-three bassoon (2) (e5) ((0 "schneller") (2 "pp"))))
	      "/E/code/ensemble/test1.xml" :tempo 70 :time-sig '(5 8))
|#
(defun lists-to-xml (lists file &key (time-sig '(4 4)) (tempo 60)
				  (title (pathname-name file)) composer)
  (unless (and (listp lists) (loop for ls in lists always (listp ls)))
    (error "lists in lists-to-xml seems to be malformed"))
  (let* (sc list-of-list-of-bars)
    (loop for i in lists
	  ;; split durations so they fit into bars and parse to (tied) events
	  for events = (durations-and-pitches-to-events
			(third i) (fourth i) time-sig tempo)
	  do (multiple-value-bind (events attacks-indices)
		 (durations-and-pitches-to-events
		  (third i) (fourth i) time-sig tempo)
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
		      collect (append bars (ml empty-bar
					       (- max-len (length bars)))))))))
    ;; put them in a slippery-chicken object:
    (loop for bars in (reverse list-of-list-of-bars) and i in lists
	  do (setf sc (bars-to-sc bars :sc sc :player (first i)
				       :instrument (second i) :tempo tempo)))
    ;; set composer and title
    (setf (composer sc) composer (title sc) title)
    (check-ties sc t)
    ;; call write-xml on sc-object
    (write-xml sc :file file)))

;; EOF lists-to-xml.lsp
