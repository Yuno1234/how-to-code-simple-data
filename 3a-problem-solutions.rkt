;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 3a-problem-solutions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;============================================================
;; cat
;;============================================================
;; Constants:

(define WIDTH 600)
(define HEIGHT 400)

(define CTR-Y (/ HEIGHT 2))

(define SPEED 3)

(define MTS (empty-scene WIDTH HEIGHT))

(define CAT-IMG (scale 0.3 (bitmap "img/cat-img.png")))

  
;; =================
;; Data definitions:

; Cat is Number
; interp. x position of the cat in screen coordinates
(define C1 0)
(define C2 (/ WIDTH 2))
(define C3 WIDTH)
#;
(define (fn-for-cat c)
  (... c))

; Template rules used:
;  - atomic non-distinct: Number


;; =================
;; Functions:

;; Cat -> Cat
;; start the world with (main 0)

(define (main c)
  (big-bang c                          ; Cat
            (on-tick   advance-cat)    ; Cat -> Cat
            (to-draw   render)         ; Cat -> Image
            (on-key    handle-key)     ; Cat KeyEvent -> Cat
            (on-mouse  handle-mouse))) ; Cat Integer Integer MouseEvent -> Cat


;; Cat -> Cat
;; produce the next cat, by advancing it 1 pixel to right
(check-expect (advance-cat 3) (+ 3 SPEED))

; (define (advance-cat c) 0)  ;stub

;<use template from Cat>

(define (advance-cat c)
  (+ c SPEED))


;; Cat -> Image
;; render the cat image at appropriate place on MTS
(check-expect (render 4) (place-image CAT-IMG 4 CTR-Y MTS))

; (define (render c) MTS)    ;stub

;<use template from Cat>

(define (render c)
  (place-image CAT-IMG c CTR-Y MTS))


;; Cat KeyEvent -> Cat
;; reset cat to left edge when space key is pressed
(check-expect (handle-key 10 " ")  0)
(check-expect (handle-key 10 "a") 10)
(check-expect (handle-key  0 " ")  0)
(check-expect (handle-key  0 "a")  0)

;(define (handle-key c ke) 0)   ;stub

(define (handle-key c ke)
  (cond [(key=? ke " ") 0]
        [else c]))


;; Cat MouseEvent -> Cat
;; move cat to x-axis of mouse when left mouse is clicked
(check-expect (handle-mouse 10 20 CTR-Y "button-down") 20)
(check-expect (handle-mouse 10 20 CTR-Y "button-up") 10)
(check-expect (handle-mouse 10 10 CTR-Y "button-down") 10)
(check-expect (handle-mouse 10 10 CTR-Y "button-up") 10)

;(define (handle-mouse c x y me) 0)  ;stub

(define (handle-mouse c x y me)
  (cond [(mouse=? me "button-down") x]
        [else c]))



;;============================================================
;;countdown-animation
;;============================================================
;; Constants:

(define WIDTH 600)
(define HEIGHT 400)

(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))

(define MTS (empty-scene WIDTH HEIGHT))

(define TEXT-SIZE 200)
(define TEXT-COLOR "black")

;; =================
;; Data definitions:

; Count is Integer[10, 0]
; interp. text image of count displayed
(define C1 10)
(define C2 5)
(define C3 0)
#;
(define (fn-for-count c)
  (... c))

; Template rules used:
;  - atomic non-distinct: Integer[10, 0]

;; =================
;; Functions:

;; Count -> Count
;; start the world with (main 10)
;; 
(define (main c)
  (big-bang c                            ; Count
            (on-tick   advance-count 1)  ; Count -> Count
            (to-draw   render)           ; Count -> Image
            (on-key    handle-key)))     ; Count KeyEvent -> Count


;; Count -> Count
;; produce the next count, by advancing subtracting 1 from current count
(check-expect (advance-count 10) 9)
(check-expect (advance-count  3) 2)
(check-expect (advance-count  0) 0)

; (define (advance-count c) 0)   ;stub

;<use template from Count>

(define (advance-count c)
  (if (> c 0)
      (- c 1)
      0))


;; Count -> Image
;; render the count text image at the center on MTS
(check-expect (render 5) (place-image (text (number->string 5) TEXT-SIZE TEXT-COLOR) CTR-X CTR-Y MTS))

; (define (render c) MTS)        ;stub

;<use template from Count>

(define (render c)
  (place-image (text (number->string c) TEXT-SIZE TEXT-COLOR) CTR-X CTR-Y MTS))


;; Count - Count
;; reset the Count to 10 when the spacebar is pressed
(check-expect (handle-key 5  " ") 10)
(check-expect (handle-key 10 " ") 10)
(check-expect (handle-key 5  "a") 5)
(check-expect (handle-key 10 "a") 10)

;(define (handle-key c ke) 0)  ;stub

(define (handle-key c ke)
  (cond [(key=? ke " ") 10]
        [else c]))
