;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 3b-problem-solutions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;================================================
;; compound
;; ===============================================
;; Data definitions:

(define-struct player (fn ln))
; Player is (make-player String String)
; interp. (make-player fn ln) is a hockey player with
;          fn is the first name
;          ln is the last name
(define P1 (make-player "Bobby" "Orr"))
(define P2 (make-player "Wayne" "Gretzky"))

(define (fn-for-player p)
  (... (player-fn p)    ;String
       (player-ln p)))  ;String

; Template rules used:
;  - Compound: 2 fields


;;================================================
;; movie
;; ===============================================
;; Data definitions:

(define-struct movie (title budget released))
; Movie is (make-movie String Natural Natural)
; interp. (make-movie title budget year) is a hocky player with
;          title is the titel of the movie
;          budget is the budget of the movie
;          year is the year of the movie
(define M1 (make-movie "Titanic" 200000000 1997))
(define M2 (make-movie "Avatar" 237000000 2009))
(define M3 (make-movie "The Avengers" 220000000 2012))
(define M4 (make-movie "Life of Pi" 120000000 2012))

(define (fn-for-movie m)
  (... (movie-title m)    ;String
       (movie-budget m)   ;Natural
       (movie-released m)))   ;Natural

; Template rules used:
;  - Compound: 3 fields


;; =================
;; Functions:

; Movie Movie -> String
; compare two movies and produces the title of the most recently released movie
(check-expect (recent-movie M1 M2) "Avatar")
(check-expect (recent-movie M3 M2) "The Avengers")
(check-expect (recent-movie M3 M4) "released in the same year")

;(define (recent-movie m1 m2) m1) ;stub

#;
(define (fn-for-movie m1 m2)
  (... (movie-title m1)
       (movie-budget m1)
       (movie-year m1)
       (movie-title m2)
       (movie-budget m2)
       (movie-year m2)))

(define (recent-movie m1 m2)
  (cond [(> (movie-released m1) (movie-released m2)) (movie-title m1)]
        [(< (movie-released m1) (movie-released m2)) (movie-title m2)]
        [else "released in the same year"]))



;;================================================
;; student
;;================================================
;; Data definitions:

(define-struct student (name grade allergies?))
; Student is (make-student String Natural[1, 12] Boolean) 
; interp. student with a name, a grade between 1 to 12, whether or not they have alergies
(define S1 (make-student "Tom" 1 false))
(define S2 (make-student "Joe" 6 true))
(define S3 (make-student "Mia" 12 false))
(define S4 (make-student "Emma" 9 true))

(define (fn-for-student s)
  (... (student-name s)
       (student-grade s)
       (student-allergies? s)))

; Template rules used:
;  - Compound: 3 fields


;; =================
;; Functions:

; Student -> Boolean
; check whether the student is in grade 6 or below and has alergies
(check-expect (add-name S1) false)
(check-expect (add-name S2) true)
(check-expect (add-name S1) false)
(check-expect (add-name S1) false)

;(define (add-name s) S1)  ;stub

;<use template from student>

(define (add-name s)
  (and (<= (student-grade s) 6)
       (student-allergies? s)))



;;============================================================
;; cowabunga
;;============================================================
;; Constants:

(define WIDTH 400)
(define HEIGHT 200)

(define CTR-Y (/ HEIGHT 2))

(define RCOW (bitmap "img/rcow-img.png"))
(define LCOW (bitmap "img/lcow-img.png"))

(define MTS (empty-scene WIDTH HEIGHT))

;; =================
;; Data definitions:

(define-struct cow (x dx))
;; Cow is (make-cow Natural[0, WIDTH], Integer)
;; interp. (make-cow x dx) is a cow with x coordinate x and velocity dx
;;          the x is the center of the cow
;;          x is in screen coordiantes (pixels)
;;          dx is in pixels per tick
  
(define C1 (make-cow 10 3))  ; at 10, moving left -> right
(define C2 (make-cow 20 -4))  ; at 20, moving left <- right

(define (fn-for-cow c)
  (... (cow-x c)     ;Natural[0, WIDTH]
       (cow-dx c)))  ;Integer

;; Template rules used:
;;  - compound: 2 fields
  
;; =================
;; Functions

;; Cow -> Cow
;; called to make the cow go for a walk; start with (main (make-cow 0 3))
;; no tests for main function
(define (main ws)
  (big-bang ws
            (on-tick   next-cow)       ; Cow -> Cow
            (to-draw   render-cow)         ; Cow -> Image
            (on-key    handle-key)))   ; Cow KeyEvent -> Cow

  
;; Cow -> Cow
;; increase cow x by dx; bounce off edges
(check-expect (next-cow (make-cow 20  3)) (make-cow (+ 20 3)  3))   ;middle
(check-expect (next-cow (make-cow 20 -3)) (make-cow (- 20 3) -3))

(check-expect (next-cow (make-cow (- WIDTH 3) 3)) (make-cow WIDTH 3))  ;reaches edge
(check-expect (next-cow (make-cow 3 -3)) (make-cow 0 -3))

(check-expect (next-cow (make-cow (- WIDTH 2) 3)) (make-cow WIDTH -3)) ;tries to pass edge
(check-expect (next-cow (make-cow 2 -3)) (make-cow 0 3))

;(define (next-cow c) c)     ;stub

(define (next-cow c)
  (cond[(> (+ (cow-x c) (cow-dx c)) WIDTH) (make-cow WIDTH (- (cow-dx c)))]
       [(< (+ (cow-x c) (cow-dx c)) 0)     (make-cow 0     (- (cow-dx c)))]
       [else
        (make-cow (+ (cow-x c) (cow-dx c))
                  (cow-dx c))])) 
  

;; Cow -> Image
;; place appropriate cow image on MTS at (cow-x c) and CTR-Y
(check-expect (render-cow (make-cow 99 3))
              (place-image RCOW 99 CTR-Y MTS))
(check-expect (render-cow (make-cow 99 -3))
              (place-image LCOW 99 CTR-Y MTS))

;(define (render-cow c) MTS)   ;stub

(define (render-cow c)
  (place-image (choose-image c) (cow-x c) CTR-Y MTS))


;; Cow -> Image
;; produce RCOW or LCOW depending on direction cow is going; LCOW if dx = 0
(check-expect (choose-image (make-cow 10  3)) RCOW)
(check-expect (choose-image (make-cow 11 -3)) LCOW)
(check-expect (choose-image (make-cow 11  0)) LCOW)

;(define (choose-image c) RCOW) ;stub

(define (choose-image c)
  (if (> (cow-dx c) 0)
      RCOW
      LCOW))

;; Cow KeyEvent -> Cow
;; reverse direction of cow travel when space bar is pressed
(check-expect (handle-key (make-cow 10  3) " ") (make-cow 10 -3))
(check-expect (handle-key (make-cow 10 -3) " ") (make-cow 10  3))
(check-expect (handle-key (make-cow 10  3) "a") (make-cow 10  3)) 
(check-expect (handle-key (make-cow 10 -3) "a") (make-cow 10  -3)) 

;(define (handle-key c ke) c)  ;stub

(define (handle-key c ke)
  (cond [(key=? ke " ") (make-cow (cow-x c) (- (cow-dx c)))]
        [else c]))

