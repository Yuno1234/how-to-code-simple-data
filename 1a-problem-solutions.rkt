;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 1a-problem-solutions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; pythag
(sqrt (+ (sqr 3) (sqr 4)))


;; function-definitions
;(above (circle 40 "solid" "red")
;       (circle 40 "solid" "yellow")
;       (circle 40 "solid" "green"))

(define (bulb c) (circle 40 "solid" c))

(above (bulb "red") (bulb "yellow") (bulb "green"))


;; more-arithmetic-expression
(* 3 5 7)

(* (* 3 5) 7)


;; tile
(above (beside (square 20 "solid" "blue")
               (square 20 "solid" "yellow"))
       (beside (square 20 "solid" "yellow")
               (square 20 "solid" "blue")))


;; compare-images
(define IMAGE1 (rectangle 10 15 "solid" "red"))
(define IMAGE2 (rectangle 15 10 "solid" "red"))

(> (image-height IMAGE1)
   (image-height IMAGE2))

(< (image-width IMAGE1)
   (image-width IMAGE2))

(and (= (image-width IMAGE1) 
        (image-width IMAGE2)) 
     (= (image-height IMAGE1)
        (image-height IMAGE2)))


;; more-foo-evaluation
(foo (+ 3 4))

(foo 7)

(* 7 7)

49


;; function-writing

(define (larger x y)
  (if (> x y)
      x
      y))


;; foo-evaluation
(foo (substring "abcde" 0 3))

(foo "abc")

(if (string=? (substring "abc" 0 1) "a")
    (string-append "abc" "a")
    "abc")

(if (string=? "a" "a")
    (string-append "abc" "a")
    "abc")

(if true
    (string-append "abc" "a")
    "abc")

(string-append "abc" "a")

"abca"

