;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname traffic-light) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;============================================================
;; traffic-light
;;============================================================
;; Constants:
 (define RADIUS 20)
 (define SPACING 5)

 (define SPACE (square SPACING "solid" "black"))
 
 (define BACKGROUND (rectangle (+ (* SPACING 2) (* RADIUS 2))
                               (+ (* SPACING 4) (* RADIUS 6))
                               "solid"
                               "black"))
 (define R-ON (overlay (above SPACE
                              (circle RADIUS "solid" "red")
                              SPACE
                              (circle RADIUS "outline" "yellow")
                              SPACE
                              (circle RADIUS "outline" "green")
                              SPACE)
                       BACKGROUND))

 (define Y-ON (overlay (above SPACE
                              (circle RADIUS "outline" "red")
                              SPACE
                              (circle RADIUS "solid" "yellow")
                              SPACE
                              (circle RADIUS "outline" "green")
                              SPACE)
                       BACKGROUND))

 (define G-ON (overlay (above SPACE
                              (circle RADIUS "outline" "red")
                              SPACE
                              (circle RADIUS "outline" "yellow")
                              SPACE
                              (circle RADIUS "solid" "green")
                              SPACE)
                       BACKGROUND))
 


;; =================
;; Data definitions:

;; TraficLight is one of:
;   - "red"
;   - "yellow"
;   - "green"
; inter. colors of the trafic light
; <examples are redundant for enumerations>

(define (fn-for-trafic-light l)
  (cond [(string=? l "red") (...)]
        [(string=? l "yellow") (...)]
        [(string=? l "green") (...)]))

; Templates rules used:
;  - one of: 3cases
;  - atomic distinct: "red"
;  - atomic distinct: "yellow"
;  - atomic distinct: "green"

;; =================
;; Functions:

;; TL -> TL
;; start the world with (main "red")
;; 
(define (main l)
  (big-bang l                          ; TL
            (on-tick   next-color 1)   ; TL -> TL
            (to-draw   render)))       ; TL -> Image


;; TL -> TL
;; produce the next trafic light
(check-expect (next-color "red") "green")
(check-expect (next-color "green") "yellow")
(check-expect (next-color "yellow") "red")

;(define (next-color l) "yellow") ;stub

(define (next-color l)
  (cond [(string=? l "red") "green"]
        [(string=? l "yellow") "red"]
        [(string=? l "green") "yellow"]))


;; TL -> Image
;; render the Image for the next trafic light
(check-expect (render "red") R-ON)
(check-expect (render "yellow") Y-ON)
(check-expect (render "green") G-ON)
    
;(define (render l) Y-ON) ; stub

(define (render l)
  (cond [(string=? l "red") R-ON]
        [(string=? l "yellow") Y-ON]
        [(string=? l "green") G-ON]))