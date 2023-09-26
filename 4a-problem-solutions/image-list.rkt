;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname image-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;;============================================================
;; image-list
;;============================================================
;; Data definitions:

; ListOfImage if one of:
;  - empty
;  - (cons Image ListOfImage)
; interp. list of images
(define LOI1 empty)
(define LOI2 (cons (square 40 "solid" "slateblue")
                   (cons (circle 30 "outline" "red")
                         (cons (rectangle 40 20 "outline" "black")
                               empty))))
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))

; Template rules used:
;  - one of: 2 cases
;  - atomic distinct: empty
;  - compound: (cons Number ListOfImage)
;  - self-reference: (rest loi) is ListOfImage


;; =================
;; Functions:

; ListOfImages -> Number
; produces total area of the images in the list
(check-expect (total-area empty) 0)
(check-expect (total-area (cons (square 4 "solid" "slateblue") empty)) 16)
(check-expect (total-area (cons (rectangle 2 4 "solid" "red")
                                (cons (square 3 "solid" "blue")
                                      empty)))
              17)

;(define (total-area loi) 0)
;<template from ListOfImage>

(define (total-area loi)
  (cond [(empty? loi) 0]
        [else
         (+ (* (image-width (first loi))
               (image-height (first loi)))
            (total-area (rest loi)))]))

