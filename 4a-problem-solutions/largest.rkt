;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname largest) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;============================================================
;; largest
;;============================================================
;; Data definitions:

;; ListOfNumber is one of:
;;  - empty
;;  - (cons Number ListOfNumber)
;; interp. a list of numbers
(define LON1 empty)
(define LON2 (cons 60 (cons 42 empty)))
#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Number ListOfNumber)
;;  - self-reference: (rest lon) is ListOfNumber

;; =================
;; Functions:

; ListOfNumber -> Number
; produce the largest number in the list (produce 0 if empty)
(check-expect (largest empty) 0)
(check-expect (largest (cons 60 (cons 42 empty))) 60)
(check-expect (largest (cons 17 (cons 30 empty))) 30)
(check-expect (largest (cons 10 (cons 20 (cons 50 empty)))) 50)

;(define (largest lon) 0)  ;stub
;<template from ListOfNumber>

(define (largest lon)
  (cond [(empty? lon) 0]
        [else
         (if (> (first lon) (largest (rest lon)))
                (first lon)
                (largest (rest lon)))]))
