;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname flower) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;============================================================
;; flower
;;============================================================
;; Constants:
(define WIDTH 400)
(define HEIGHT 300)

(define MTS (empty-scene WIDTH HEIGHT "yellowgreen"))

(define CENTER (circle 10 "solid" "yellow"))
(define PETAL (ellipse 15 30 "solid" "pink"))
(define FLOWER (overlay CENTER
                        (above PETAL (square 10 "solid" "yellow") PETAL)
                        (rotate 60 (above PETAL (square 10 "solid" "yellow") PETAL))
                        (rotate 120 (above PETAL (square 10 "solid" "yellow") PETAL))))

(define GROWTH-RATE 2)

;; =================
;; Data definitions:

(define-struct flower (x y size))
; flower is (make-flower Integer Integer Natural)
;  .interp a flower with x position, y position and a side length.
(define F1 (make-flower 0 0 0))
(define F2 (make-flower (/ WIDTH 2) (/ HEIGHT 2) 20))

(define (fn-for-flower f)
  (... (flower-x f)       ;Int
       (flower-y f)       ;Int
       (flower-size f)))  ;Natural

;; Template rules used:
;;  - compound: 3 fields


;; =================
;; Functions:

;; WS -> WS
;; start the world with (main (make-flower (/ WIDTH 2) (/ HEIGHT 2) 0))
;; 
(define (main ws)
  (big-bang ws                          ; Flower
            (on-tick   next-flower)     ; Flower -> Flower
            (to-draw   render-flower)   ; Flower -> Image
            (on-mouse  handle-mouse)))      ; Flower Integer Integer MouseEvent -> Flower

;; Flower -> Flower
;; produce the next ...
(check-expect (next-flower (make-flower 0 0 0)) (make-flower 0 0 (+ 0 GROWTH-RATE)))
(check-expect (next-flower (make-flower 20 50 30)) (make-flower 20 50 (+ 30 GROWTH-RATE)))
(check-expect (next-flower (make-flower 20 50 100)) (make-flower 20 50 100))

;(define (next-flower f) f)  ;stub

;<used template from flower>

(define (next-flower f)
  (if (< (flower-size f) 100)
      (make-flower (flower-x f)
                   (flower-y f)       
                   (+ (flower-size f) GROWTH-RATE))
      f))


;; Flower -> Image
;; render flower at the appropriate position on MTS at the correct x, y coordinate and of the given size
(check-expect (render-flower (make-flower 0 0 0))
              (place-image empty-image 0 0 MTS))
(check-expect (render-flower (make-flower 20 50 30))
              (place-image (scale (/ 30 100) FLOWER) 20 50 MTS))
(check-expect (render-flower (make-flower 20 50 100))
              (place-image (scale (/ 100 100) FLOWER) 20 50 MTS))

(define (render-flower f)
  (if (= (flower-size f) 0)
      (place-image empty-image (flower-x f) (flower-y f) MTS)
      (place-image (scale (/ (flower-size f) 100) FLOWER) (flower-x f) (flower-y f) MTS)))

;; Flower Integer Integer MouseEvent -> Flower
;; produce flower at the appropriate position for according to the mouse event
(check-expect (handle-mouse (make-flower 20 50 30) 0 0 "button-down") (make-flower 0 0 0))
(check-expect (handle-mouse (make-flower 20 50 30) 0 0 "button-up") (make-flower 20 50 30))

(define (handle-mouse f x y me)
  (cond [(mouse=? me "button-down") (make-flower x y 0)]
        [else f]))