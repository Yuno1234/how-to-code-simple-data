;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2-problem-solutions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; cond

(define (absval n)
  (cond [(> n 0) n]
        [(< n 0) (* -1 n)]
        [else 0]))

(absval -3)

(cond [(> -3 0) -3]
      [(< -3 0) (* -1 -3)]
      [else 0])

(cond [false -3]
      [(< -3 0) (* -1 -3)]
      [else 0])

(cond [(< -3 0) (* -1 -3)]
      [else 0])

(cond [true (* -1 -3)]
      [else 0])

(* -1 -3)

3


;;================================================
;; city-name

;; Data definitions:

; CityName is String
; interp. the name of a city
(define CN1 "Boston")
(define CN2 "Vancouver")
#;
(define (fn-for-city-name cn)
  (... cn))

; Template rules used:
; - atomic non-distinct: String


;; Functions:

; CityName -> Boolean
; produce true if the given city is Hogsmeade
(check-expect (best? "Boston") false)
(check-expect (best? "Hogsmeade") true)


;(define (best? cn) false)   ;stub

; took template from CityName

(define (best? cn) (string=? cn "Hogsmeade"))


;;================================================
;; seat-num

;; Data definitions:

; SeatNumber is Natrual[1, 32]
; interp. seat numbers in a row, 1 and 32 are aisle seats
(define S1  1)  ;aisle
(define S2 16)  ;middle
(define S3 32)  ;aisle
#;
(define (fn-for-seatnumber sn)
  (... sn))

; Template rules used:
; - atomic non-distinct: Natrual[1, 32]


;; Functions:

; SeatNum -> Boolean
; produce true if the given seat number is on the aisle
(check-expect (aisle? 1) true)
(check-expect (aisle? 16) false)
(check-expect (aisle? 32) true)

;(define (aisle? sn) false)   ;stub

;<use template from SeatNum>

(define (aisle? sn)
  (or (= sn 1)
      (= sn 32)))



;;================================================
;; letter-grade

;; Data definitions:

; LetterGrade is one of:
;  - "A"
;  - "B"
;  - "C"
; interp. the letter grade in a course
; <examples are redundant for enumerations>
#;
(define (fn-fro-letter-grade lg)
  (cond [(string=? lg "A")  (...)]
        [(string=? lg "B")  (...)]
        [(string=? lg "C")  (...)]))

; Template rules used:
;  - one of: 3 cases
;  - atomic distinct: "A"
;  - atomic distinct: "B"
;  - atomic distinct: "C"


;; Functions:

; LetterGrade -> LetterGrade
; produce next highest letter grade (no change for A)
(check-expect (bump-up "A") "A")
(check-expect (bump-up "B") "A")
(check-expect (bump-up "C") "B")

;(define (bump-up lg) "A")   ;stub

;<use template from LetterGrade>

(define (bump-up lg)
  (cond [(string=? lg "A")  "A"]
        [(string=? lg "B")  "A"]
        [(string=? lg "C")  "B"]))


;;================================================
;; countdown

;; Data definitions:

; CountDown is one of:
;  - false
;  - Natural[1, 10]
;  - "complete"
; interp.
;    false            means countdown has not yet started
;    Natural[1, 10]   means countdown is running and how many seconds left
;    "complete"       means countdown is over
(define CD1 false)
(define CD2 10)
(define CD3 1)
(define CD4 "complete")
#;
(define (fn-for-countdown c)
  (cond [(false? c) (...)]
        [(number? c) (... c)]
        [else (...)]))

; Template rules used:
;  - one of: 3 cases
;  - atomic distinct: false
;  - atomic non-distinct: Natural[1, 10]
;  - atomic distinct: "complete"


;; Functions:

; Countdown -> Image
; produce nice image of current state of coutdown
(check-expect (countdown-to-image false) (square 0 "solid" "white"))
(check-expect (countdown-to-image 5) (text (number->string 5) 24 "black"))
(check-expect (countdown-to-image "complete") (text "Happy New Year!!" 24 "red"))


;(define (countdown-to-image c) (square 0 "solid" "white"))   ;stub

;<use template from Countdown>

(define (countdown-to-image c)
  (cond [(false? c)
         (square 0 "solid" "white")]
        [(and (number? c) (<= 1 c) (<= c 10))
         (text (number->string c) 24 "black")]
        [else
         (text "Happy New Year!!" 24 "red")]))


;;================================================
;; demolish

;; Data definitions:

; BuildingStatus is one of:
;  - "new"
;  - "old"
;  - "heritage"
; interp. classification levels that will classify buildings based on how old they are
; <examples are redundant for enumurations>
#;
(define (fn-for-building-status bs)
  (cond [(string=? "new" bs) (...)]
        [(string=? "old" bs) (...)]
        [(string=? "heritage" bs) (...)]))

; Template rules used:
;  - one of: 3 cases
;  - atomic distinct: "new"
;  - atomic distinct: "old"
;  - atomic distinct: "heritage"


;;Functions:

; BuildingStatus -> Boolean
; determine whether a building should be torn down or not. ("old" buildings are demolished)
(check-expect (demolish "new") false)
(check-expect (demolish "old") true)
(check-expect (demolish "heritage") false)

;(define (demolish bs) false)   ;stub

;<use template from BuildingStatus>

(define (demolish bs)
  (cond [(string=? "new" bs) false]
        [(string=? "old" bs) true]
        [(string=? "heritage" bs) false]))


;;================================================
;; rocket-starter

; Data definitions:

; RocketDescent is one of:
;  - Number[100, 1]
;  - false
; .interp
;     Number[100, 1]     means the rocket is descent with in 100km
;     false              means the rocket has landed
(define RD1 100)
(define RD2 50)
(define RD3 1)
(define RD4 0.5)
(define RD5 false)
#;
(define (fn-for-rocket-descent d)
  (cond [(and (number? d)
              (> d 0)
              (>= 100 d))
         (... d)]
        [else (...)]))

; Template rules used:
;  - one of: 2 cases
;  - atomic non-distinct: Number[100, 1]
;  - atomic distinct: false


;; Functions:

; RocketDescent -> String
; produce a short string of remaining descent distance
(check-expect (rocket-descent-to-msg 100) "100km remaining")
(check-expect (rocket-descent-to-msg 50) "50km remaining")
(check-expect (rocket-descent-to-msg 1) "1km remaining")
(check-expect (rocket-descent-to-msg false) "The rocket has landed!")

;(define (rocket-descent-to-msg d) "")   ;stub

;<use template from Rocket>

(define (rocket-descent-to-msg d)
  (cond [(and (number? d)
              (<= d 100)
              (<= 1 d))
         (string-append (number->string d) "km remaining")]
        [else
         "The rocket has landed!"]))