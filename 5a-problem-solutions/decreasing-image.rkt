;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname decreasing-image) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;;============================================================
;; decreasing-image
;;============================================================

; Natural -> Image
; produce a image of all the Naturals from n to 0
(check-expect (decreasing-image 0) (text "0" 24 "black"))
(check-expect (decreasing-image 2) (beside/align "bottom"
                                                 (text "2" 24 "black")
                                                 (text "1" 24 "black")
                                                 (text "0" 24 "black")))

;(define (decreasing-image n) 0)  ;stub

(define (decreasing-image n)
  (cond [(zero? n) (text "0" 24 "black")]
        [else
         (beside/align "bottom"
                       (text (number->string n) 24 "black")
                       (decreasing-image (sub1 n)))]))