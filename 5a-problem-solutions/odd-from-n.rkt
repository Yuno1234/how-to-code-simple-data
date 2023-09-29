;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname odd-from-n) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;============================================================
;; odd-from-n
;;============================================================

;Natural -> ListOfNatural
;produces a list of all the odd numbers from n to 1
(check-expect (odd-from-n 0) empty)
(check-expect (odd-from-n 1) (cons 1 empty))
(check-expect (odd-from-n 3) (cons 3 (cons 1 empty)))

;(define (odd-from-n n) 1)   ;stub

(define (odd-from-n n)
  (cond [(zero? n) empty]
        [else
         (if (odd? n)
             (cons n (odd-from-n (sub1 n)))
             (odd-from-n (sub1 n)))]))