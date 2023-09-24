;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 1b-problem-solutions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; double

; Number -> Number
; produce 2 times the given number
(check-expect (double 3) 6)
(check-expect (double 4.2) 8.4)

;(define (double n) 0)  ;stub

;(define (double n)     ;template
;  (... n))            

(define (double n)      ;function body
  (* 2 n))



;; pluralize

; String -> String
; pluralize a string (adding an "s" to the end)
(check-expect (pluralize "string") "strings")
(check-expect (pluralize "") "s")

;(define (pluralize str) "")   ;stub

;(define (pluralize str)       ;template
;  (... str))

(define (pluralize str)        ;function body
  (string-append str "s"))



;; yell

; String -> String
; add "!" to the end of str
(check-expect (yell "hello") "hello!")
(check-expect (yell "bye") "bye!")

;(define (yell str) "")      ;stub

;(define (yell str)          ;template
;  (... str))

(define (yell str)           ;function body
  (string-append str "!"))



;; area

; Number -> Number
; given length of one side of square, produce the area of the square
(check-expect (area 3) 9)
(check-expect (area 3.2) (* 3.2 3.2))

;(define (area s) 0)   ;stub

;(define (area s)      ;template
;  (... s))

(define (area s)       ;function body
  (* s s))



;; image-area

; Image -> Natrual
; produce images's widht * height (area)
(check-expect (image-area (rectangle 2 3 "solid" "red")) (* 2 3))

;(define (image-area img) 0)  ;stub

;(define (image-area img)     ;template
;  (... img))                 

(define (image-area img)      ;function body
  (* (image-width img) (image-height img)))



;; tall

; Image -> Boolean
;produce true if the image is tall (height is greater than width)
(check-expect (tall? (rectangle 2 3 "solid" "red")) true)
(check-expect (tall? (rectangle 3 2 "solid" "red")) false)
(check-expect (tall? (rectangle 3 3 "solid" "red")) false)

;(define (tall? img) false)   ;stub

;(define (tall? img)          ;template
;  (... img))

(define (tall? img) (> (image-height img) (image-width img)))  ;function body



;;less-than-five

; String -> Boolean
; produce true if length of s is less than 5
(check-expect (less-than-5? "") true)
(check-expect (less-than-5? "five") true)
(check-expect (less-than-5? "12345") false)
(check-expect (less-than-5? "eighty") false)

;(define (less-than-5? s)  ;stub
;  true)

;(define (less-than-5? s)  ;template
;  (... s))

(define (less-than-5? s)   ;function body
  (< (string-length s) 5))



;; boxify

;; Image -> Image
;; Puts a box around given image. Box is 2 pixels wider and taller than given image.
;; NOTE: A solution that follows the recipe but makes the box the same width and height 
;;       is also good. It just doesn't look quite as nice. 
(check-expect (boxify (circle 10 "solid" "red")) 
              (overlay (rectangle 22 22 "outline" "black")
                       (circle 10 "solid" "red")))
(check-expect (boxify (star 40 "solid" "gray")) 
              (overlay (rectangle 67 64 "outline" "black")
                       (star 40 "solid" "gray")))

;(define (boxify i) (circle 2 "solid" "green"))

#;
(define (boxify i)
  (... i))

(define (boxify i)
  (overlay (rectangle (+ (image-width  i) 2)
                      (+ (image-height i) 2)
                      "outline"
                      "black")
           i))



;; double-error

;; Number -> Number
;; doubles n
(check-expect (double 0) 0)
(check-expect (double 4) 8)
(check-expect (double 3.3) (* 2 3.3))
(check-expect (double -1) -2)

;(define (double n) 0)

#;
(define (double n)
  (... n))

(define (double n)
  (* 2 n))