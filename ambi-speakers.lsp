;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             ambi-speakers.lsp
;;;
;;; Project:          None but see below for details and usage
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    14th September 2021
;;;
;;; $$ Last modified:  16:22:47 Thu Apr 17 2025 CEST
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)


#| 

Calculating angles necessary for ambisonic decoding in Common Lisp.

Useful paper:

"How to make Ambisonics sound good" (Matthias Frank) Institute of Electronic
Music and Acoustics, University of Music and Performing Arts, Graz, Austria:

"Physically, the most accurate Ambisonics system would employ a mode-matching
decoder with basic weighting and delay compensation in an anechoic room."

"Delay compensation of differently distant loudspeakers towards the central
listening position creates a physically accurate sound field around this posi-
tion. However, the accurate area is typically smaller than a human head at high
frequencies. Thus, small head movements around the central listening position
can yield strong phase variations that cause severe changes in coloration and
localization"

"A large number density of loudspeakers can yield a good localization
performance, especially at off-center positions. However, the order has to be
chosen accordingly. Too low orders activate too many neighboring
loudspeakers with the same signal resulting in coloration. This holds true for
in-phase weighting that exaggerates side lobe attenuation. Too high orders can
cause imbalanced timbre, source width and loudness, especially when employing
traditional decoder strategies on irregular loudspeaker arrangements."

"The author knows from personal experience that 4th or 5th order max-rE-weighted
AllRAD-decoded Ambisonics on hemispheres with about 20-30 loudspeakers
works well. Without delay compensation in studio-like rooms, such a system
offers a perceptual sweet-spot of about 2/3 of the loudspeaker array's radius,
i.e. 30 listeners for an array radius of 3m."


--------------------------------------------------------------------------------
Following parameters are needed.

height: the height of the speaker from the mixer's ears 

floor-distance: from the mixer's ears, the distance to the position directly
under the loudspeaker (or at it if the speaker is on the floor), i.e.,
diagonally along the floor.

floor-x: the distance along the x-axis on the ground plane from the mixer's ears
to the position directly below the speaker (e.g. from stage middle to speaker
left along the edge of the stage, in a standard stereo pair). If to the left
of the mixer, use a positive number, if to the right, negative.

quadrant-offset: how many degrees to offset the calulated azimuth. E.g. for the
front left-right pair this will be 0 (and the floor-x for left will be
positive, for right, negative), for the rear it will be 180 (and for rear right
floor-x will be positive because if you turn 180 degrees it's on your left).

Using the IEM AllRAD approach, radius and gain only count for the virtual
speakers so we don't bother with these in our calculations rather we adjust gain
manually on the desk for each speaker. 

For each speaker we need:

- azimuth: angle from front centre on the ground plane, in degrees from 0
  counter-clockwise

- elevation: angle from floor to speaker on the height plane

We can first calculate the elevation: 
(clm::radians->degrees (atan (/ height floor-distance)))

The azimuth is then:
(clm::radians->degrees (asin (/ floor-x floor-distance)))

AllRADecoder file from 13.9.21 is ~/mix/Ambisonic Production
Templates/neue-aula-symposium-decoder1.json whereas the one with these lisp
calculations is neue-aula-symposium-decoder2.json

Trig help for those who have forgotten everything they learned at school:
http://www.csgnetwork.com/trigtriformulatables.html

(load "/Users/michael/ic/folkwang/github-icem/Lisp/ambi-speakers.lsp")
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prints useful data for importing/typing speaker positions into IEM and SAD-P
;;; decoders as well as the ICST Max externals in spherical AED (azimuth,
;;; elevation, distance) and Cartesian XYZ formats
(defun ambi-speakers
    (&optional
       ;; this was the data from the Neue Aula, 14th September 2021, which we
       ;; can leave for now for historical reflections:xs
       ;;
       ;; height floor-distance floor-x quadrant-offset (degrees)
       (speakers '((1 (6.6 14.5 5.5 0))
                   (2 (6.6 14.5 -5.5 0))
                   (3 (6.6 10 7 180))   
                   (4 (6.6 10 -7 180))  
                   (5 (3.5 13.3 6.2 0)) 
                   (6 (3.5 13.3 -6.2 0))
                   (7 (0.8 11.3 5.3 180))
                   (8 (0.8 11.3 -5.3 180))
                   (9 (0.8 11.3 0 0))
                   (10 (0.9 10.4 0 270))
                   (11 (0.8 10.3 0 180))
                   (12 (1.5 5.4 0 90))
                   (13 (5.7 3.5 1.175 0))
                   (14 (5.7 3.5 -1.175 0))
                   (15 (5.7 1 1.175 180))
                   (16 (5.7 1 -1.175 180)))))
  ;; MDE Tue Oct 31 13:56:59 2023, Heidhausen -- note that the IEM
  ;; AllRadecoder seems to overwrite the given channel and use instead the
  ;; number order in which they appear, so sort by channel first.
  (setq speakers (sort speakers #'< :key #'first))
  (loop for speaker in speakers do
           (when (> (count speaker speakers :test #'equalp) 1)
             (error "found duplicates of ~a" speaker)))
  (let (temp elevation azimuth qazi icst)
    (print "IEM (anti-clockwise):")
    (setq icst
          (loop for ls in speakers
                for channel = (first ls)
                for data = (second ls)
                for height = (first data)
                for floor-distance = (second data)
                for floor-x = (third data)
                for quadrant = (fourth data)
                do
                   ;; reverse floor-distance and floor-x if floor-distance <
                   ;; floor-x otherwise we get complex results for azimuth
                   ;; (inverse sines of > 1)
                   (when (< floor-distance (abs floor-x))
                     (setq temp floor-distance
                           floor-distance floor-x
                           floor-x temp))
                   (setq elevation (clm::radians->degrees
                                    ;; abs because we're not actually using
                                    ;; speakers below the floor (as yet)
                                    (atan (/ height (abs floor-distance))))
                         azimuth (clm::radians->degrees
                                  (asin (/ floor-x floor-distance)))
                         qazi (+ quadrant azimuth))
                ;; this is where we print the data that can be copy/pasted into
                ;; a .json file for import into the IEM decoders. Todo: write
                ;; the actual file.
                   (format t "~&      {")
                ;; MDE Wed Nov 15 14:54:14 2023, Heidhausen -- no degrees > 180 
                   (format t "~%        \"Azimuth\": ~,3f," (if (> qazi 180)
                                                                (- qazi 360)
                                                                qazi))
                   (format t "~%        \"Elevation\": ~,3f," elevation)
                   (format t "~%        \"Radius\": 1.0,~
                              ~%        \"IsImaginary\": false,~
                              ~%        \"Channel\": ~a,~
                              ~%        \"Gain\": 1.0~%      }," channel)
                   ;; this should help with ICST's ambisonics (which has azimuth
                   ;; clockwise rather than anti-clockwise
                collect (list channel (- qazi) elevation)))
    (terpri)
    (loop for l in icst do
             (format t "aed ~a ~,3f ~,3f 1.0, " (first l) (second l) (third l)))
    (print "ICST (clockwise) Format:")
    (print (mapcar #'(lambda (triple) (list (decimal-places (second triple) 3)
                                          (decimal-places (third triple) 3)))
                   icst))))

;; NB can't handle the colon after ambi:
(defun ambimon-dump (list &optional (rears t))
  (loop for i from 1 while list do
       (when (or rears (not (member i '(7 8))))
         (format t "~,3f ~,3f ~,3f, "
                 (nth 3 list) (nth 4 list) (nth 5 list)))
       (setq list (nthcdr 7 list))))

;;; get the diagonal floor distance from Jorge's X,Y,Z
(defun jorge-to-ambi (speaker)
  (let* ((xyz (second speaker))
         ;; for Jorge negative x is West (left) but we need the opposite
         (x (first xyz))
         (y (second xyz))
         (rear (< y 0))
         (z (third xyz)))
    ;; we have X,Y,Z and need height floor-distance floor-x
    (list (first speaker) (list z (sqrt (+ (* x x) (* y y)))
                                (if rear x (- x))
                                ;; if J's y is negative then our quadrant offset
                                ;; is 180 because the speaker is behind us
                                (if (< y 0) 180 0)))))


;;; NB Jorge does not make speakers cry but he might make them bleed ;-)
;;; force-square T will mean X and Ys will always range from -100 to 100
;;; (Tom's default, otherwise the max of X and Y will determine the extent and
;;; one dimension will not range -100->100
(defun jorge-to-sad (speakers &optional (force-square t))
  ;; assuming a flat square, we'll use the maximum X or Y to proportionally
  ;; determine all the Xs and Ys in the speakers list. Treat the height
  ;; similarly but using a separate maximum.
  (let* ((xys (flatten (mapcar #'(lambda (s) (butlast (second s))) speakers)))
         (max-flat (apply #'max (mapcar #'abs xys)))
         (zs (flatten (mapcar #'(lambda (s) (last (second s))) speakers)))
         (max-z (apply #'max (mapcar #'abs zs)))
         (max-y -999999999)
         (max-x -999999999)
         result)
    (when force-square
      (loop for x in xys by #'cddr and y in (rest xys) by #'cddr do
               (setq x (abs x)
                     y (abs y))
               (when (> x max-x)
                 (setq max-x x))
               (when (> y max-y)
                 (setq max-y y))))
    ;; print xys) (print max-x) (print max-y)
    (flet ((doit (num &optional (max max-flat))
             ;; tom doesn't use fractional values
             (round (* 100.0 (/ num max)))))
      (setq result
            (mapcar #'(lambda (s)
                        (let ((xyz (second s)))
                          (list (first s)
                                (list
                                 (doit (first xyz) (when force-square max-x))
                                 (doit (second xyz) (when force-square max-y))
                                 (doit (third xyz) max-z)))))
                    speakers)))
    ;; note that the height coordinates are not useful for the SAD-P because it
    ;; works in speaker layers rather than actual speaker 3D positions.
    (loop for speaker in result for xyz = (second speaker) do
             (format t "~&Out_~3,'0d << (~a,~a,~a) " (first speaker)
                     (first xyz) (second xyz) (third xyz)))
    result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 21.3.25: centre (5) lower and tops further away
(let ((studio1 '( ;; unterer Ring
                 (13 (-1.35 3.25 0)) ; (speaker-channel (X Y Z))
                 (14 (1.35 3.25 0))
                 (15 (3.25 1.35 0))
                 (16 (3.25 -1.35 0))
                 (17 (1.35 -3.25 0))
                 (18 (-1.35 -3.25 0))
                 (19 (-3.25 -1.35 0))
                 (20 (-3.25 1.35 0))
                 ;; Obere Ring aussen: 1-4 are the big genes: lass ich raus
                 ;; (1 (-3.25 3.05 1.25))
                 (5 (0 3.45 1.23))
                 ;; (2 (3.25 3.05 1.25))
                 (6 (3.25 0 1.6))
                 ;; (3 (3.25 -3.05 1.25))
                 (7 (0 -3.45 1.6))
                 ;; (4 (-3.25 -3.05 1.25))
                 (8 (-3.25 0 1.6))
                 ;; Obere Ring innen
                 (9 (-1.45 1.45 1.52))
                 (10 (1.45 1.45 1.52))
                 (11 (1.45 -1.45 1.52))
                 (12 (-1.45 -1.45 1.52)))))
  (print "*** Studio 1 -100<->100 range (e.g. reasurroundpan X,Y,Z values)")
  (jorge-to-sad studio1)
  (print "*** Studio 1 ambisonics:")
  (ambi-speakers (loop for speaker in studio1
                       collect (jorge-to-ambi speaker))))

(let ((neue-aula '((1 (-680 800 325))   ; main: vida L (war  (-550 1100 500))
                   (2 (680 800 325))    ; main: vida R (war (550 1100 500))
                   (3 (0 1150 550))     ; centre: Gravis 15XW
                   ;; 4 is main sub cluster
                   (5 (525 -1120 150))  ; main rears: vida R
                   (6 (-525 -1120 150)) ; main rears: vida L
                   ;; Seiten
                   (7 (-800 400 500))   ; Seite Vorne Links: gravis 15XW
                   (8 (800 400 500))    ; Seite Vorne Rechts: gravis 15XW
                   (9 (-800 -400 500))  ; Seite Hinten Links: gravis 15XW
                   (10 (800 -400 500))  ; Seite Hinten Rechts: gravis 15XW
                   (11 (800 0 500))     ; Seite Links Mitte: gravis 15XW
                   (12 (-800 0 500))    ; Seite Rechts Mitte: gravis 15XW
                   ;; Decke
                   (13 (-300 400 800))  ; Decke Links Vorn: gravis 15W
                   (14 (300 400 800))   ; Decke Rechts Vorn: gravis 15W
                   (15 (-300 -400 800)) ; Decke Links Hinten: gravis 15W
                   (16 (300 -400 800))  ; Decke Rechts Hinten: gravis 15W
                   (17 (-300 0 800))    ; Decke Links Mitte: gravis 15W
                   (18 (300 0 800))     ; Decke Rechts Vorn: gravis 15W
                   ;; additional Centres
                   (19 (-420 1100 500)) ; off Centre Left (front)
                   (20 (420 1100 500))  ; off Centre Rightt (front)
                   (21 (0 -1190 190))   ; Hinten Mitte
                   ;; now for the fills: front are all CA 106 Pros
                   (22 (-450 700 -50))     ; FF LL (extrem links)
                   (23 (-225 700 -50))     ; FF L
                   (24 (0 700 -50))        ; FF C
                   (25 (225 700 -50))      ; FF R
                   (26 (450 700 -50))      ; FF RR (extrem rechts)
                   ;; rears are all Sona 5s
                   (27 (-800 -1100 550))   ; Empore LL
                   (28 (-400 -1100 550))   ; Empore L
                   (29 (400 -1100 550))    ; Empore R
                   (30 (800 -1100 550))))) ; Empore RR
  (print "*** Neue Aula -100<->100 range (e.g. reasurroundpan)")
  (print (jorge-to-sad neue-aula))
  (print "*** Neue Aula ambisonics:")
  (ambi-speakers (mapcar #'jorge-to-ambi neue-aula)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
;;; Usage examples and old data

;;; MDE Sat Oct 21 13:00:23 2023, Heidhausen -- using the channel numbers
;;; suggested this week: mainly banks of 6. NB This is not the channel numbering
;;; used by Beschallung
;; 01: Vida Vorne L
;; 02: Vida Vorne R
;; 03: Center
;; 04: Sub
;; 05: Vida Hinten L
;; 06: Vida Hinten R

;; 07: Seite Links Vorn
;; 08: Seite Rechts Vorn
;; 09: Seite Links Hinten
;; 10: Seite Rechts Hinten
;; 11: Seite Links Mitte
;; 12: Seite Rechts Mitte

;; 13: Decke Links Vorn
;; 14: Decke Rechts Vorn
;; 15: Decke Links Hinten
;; 16: Decke Rechts Hinten
;; 17: Decke Links Mitte
;; 18: Decke Rechts Mitte

;; 19: Hinten L
;; 20: Hinten R

;; 21: Frontfill LL
;; 22: Frontfill L
;; 23: Frontfill C
;; 24: Frontfill R
;; 25: Frontfill RR
;; 26: Empore LL
;; 27: Empore L
;; 28: Empore R
;; 29: Empore RR

(let ((neue-aula-neu '((1 (-650 800 350)) ; main: vida L (war  (-550 1100 500))
                       (2 (650 800 350)) ; main: vida R (war (550 1100 500))
                       (3 (0 1100 550)) ; centre: Gravis 15XW
                       ;; 4 is main sub cluster
                       (6 (550 -1100 150)) ; main rears: vida R
                       (5 (-550 -1100 150)) ; main rears: vida L
                       (7 (-800 400 500)) ; ring VL: gravis 15XW
                       (8 (800 400 500)) ; ring VR: gravis 15XW
                       (12 (800 0 500)) ; ring MR: gravis 15XW
                       (10 (800 -400 500)) ; ring HR: gravis 15
                       (20 (200 -750 250)) ; ring BR: gravis 15XW
                       (19 (-200 -750 250)) ; ring BL: gravis 15XW
                       (9 (-800 -400 500)) ; ring HL: gravis 15XW
                       (11 (-800 0 500))  ; ring ML: gravis 15XW
                       (13 (-300 400 800))  ; top VL: gravis 15W
                       (14 (300 400 800))  ; top VR: gravis 15W
                       (18 (300 0 800))  ; top MR: gravis 15W
                       (16 (300 -400 800))  ; top HR: gravis 15W
                       (15 (-300 -400 800)) ; top HL: gravis 15W
                       (17 (-300 0 800)) ; top ML: gravis 15W
                       ;; now for the fills: front are all CA 106 Pros
                       (21 (-450 700 -50)); FF L
                       (22 (-225 700 -50)); FF ML
                       (23 (0 700 -50)); FF M
                       (24 (225 700 -50)); FF MR
                       (25 (450 700 -50)); FF R
                       ;; rears are all Sona 5s
                       (29 (800 -1100 550)); BF HR (rear/back)
                       (28 (400 -1100 550)); BF MR
                       (27 (-400 -1100 550)); BF ML
                       (26 (-800 -1100 550)); BF HL
                       )))
  (print (jorge-to-sad neue-aula-neu))
  (ambi-speakers (mapcar #'jorge-to-ambi neue-aula-neu)))

;;; von Jorge 10.1.23, leaving out the stage and balcony rear fills.
;;; MDE Sat Apr 22 13:20:00 2023, Heidhausen -- Vidas front estimated at 5m now
;;; rather than 4m 
(let ((neue-aula-neu '((1 (-650 800 350)) ; main: vida L (war  (-550 1100 500))
                       (2 (650 800 350)) ; main: vida R (war (550 1100 500))
                       (3 (0 1100 550)) ; centre: Gravis 15XW
                       ;; 4 is main sub cluster
                       (5 (550 -1100 150)) ; main rears: vida R
                       (6 (-550 -1100 150)) ; main rears: vida L
                       (7 (-800 400 500)) ; ring VL: gravis 15XW
                       (8 (800 400 500)) ; ring VR: gravis 15XW
                       (9 (800 0 500)) ; ring MR: gravis 15XW
                       (10 (800 -400 500)) ; ring HR: gravis 15
                       (11 (200 -750 250)) ; ring BR: gravis 15XW
                       (12 (-200 -750 250)) ; ring BL: gravis 15XW
                       (13 (-800 -400 500)) ; ring HL: gravis 15XW
                       (14 (-800 0 500))  ; ring ML: gravis 15XW
                       (15 (-300 400 800))  ; top VL: gravis 15W
                       (16 (300 400 800))  ; top VR: gravis 15W
                       (17 (300 0 800))  ; top MR: gravis 15W
                       (18 (300 -400 800))  ; top HR: gravis 15W
                       (19 (-300 -400 800)) ; top HL: gravis 15W
                       (20 (-300 0 800)) ; top ML: gravis 15W
                       ;; now for the fills: front are all CA 106 Pros
                       (21 (-450 700 -50)); FF L
                       (22 (-225 700 -50)); FF ML
                       (23 (0 700 -50)); FF M
                       (24 (225 700 -50)); FF MR
                       (25 (450 700 -50)); FF R
                       ;; rears are all Sona 5s
                       (26 (800 -1100 550)); BF HR (rear/back)
                       (27 (400 -1100 550)); BF MR
                       (28 (-400 -1100 550)); BF ML
                       (29 (-800 -1100 550)); BF HL
                       )))
  (print (jorge-to-sad neue-aula-neu))
  (ambi-speakers (mapcar #'jorge-to-ambi neue-aula-neu)))

;;; Studio 1: Masse von Jorge 22.4.22: erstmal die X,Y,Z Werte: X and Y is along
;;; the wall and Z is height 
(let ((studio1 '(;; unterer Ring
                 (13 (-1.35 3.25 0))
                 (14 (1.35 3.25 0))
                 (15 (3.25 1.35 0))
                 (16 (3.25 -1.35 0))
                 (17 (1.35 -3.25 0))
                 (18 (-1.35 -3.25 0))
                 (19 (-3.25 -1.35 0))
                 (20 (-3.25 1.35 0))
                 ;; Obere Ring aussen: 1-4 are the big genes: lass ich raus
                 ;; (1 (-3.25 3.05 1.25))
                 (5 (0 3.45 1.6))
                 ;; (2 (3.25 3.05 1.25))
                 (6 (3.25 0 1.6))
                 ;; (3 (3.25 -3.05 1.25))
                 (7 (0 -3.45 1.6))
                 ;; (4 (-3.25 -3.05 1.25))
                 (8 (-3.25 0 1.6))
                 ;; Obere Ring innen
                 (9 (-1 0.75 1.5))
                 (10 (1 0.75 1.5))
                 (11 (1 -0.75 1.5))
                 (12 (-1 -0.75 1.5)))))
  (ambi-speakers (loop for speaker in studio1 collect
                       (jorge-to-ambi speaker))))


;;; jorge-to-sad works for reasurroundpan also:
(let ((studio1 '(;; unterer Ring
                 (13 (-1.35 3.25 0))    ; (speaker-channel (X Y Z))
                 (14 (1.35 3.25 0))
                 (15 (3.25 1.35 0))
                 (16 (3.25 -1.35 0))
                 (17 (1.35 -3.25 0))
                 (18 (-1.35 -3.25 0))
                 (19 (-3.25 -1.35 0))
                 (20 (-3.25 1.35 0))
                 ;; Obere Ring aussen: 1-4 are the big genes: lass ich raus
                 ;; (1 (-3.25 3.05 1.25))
                 (5 (0 3.45 1.23))
                 ;; (2 (3.25 3.05 1.25))
                 (6 (3.25 0 1.6))
                 ;; (3 (3.25 -3.05 1.25))
                 (7 (0 -3.45 1.6))
                 ;; (4 (-3.25 -3.05 1.25))
                 (8 (-3.25 0 1.6))
                 ;; Obere Ring innen
                 (9 (-1.45 1.45 1.52))
                 (10 (1.45 1.45 1.52))
                 (11 (1.45 -1.45 1.52))
                 (12 (-1.45 -1.45 1.52)))))
  (jorge-to-sad studio1))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF
