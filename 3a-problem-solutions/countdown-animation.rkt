;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname countdown-animation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

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