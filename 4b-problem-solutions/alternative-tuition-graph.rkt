;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname alternative-tuition-graph) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;;============================================================
;; alternative-tuition-graph
;;============================================================
;; Constants:

(define FONT-SIZE 24)
(define FONT-COLOR "black")

(define Y-SCALE 1/200)
(define BAR-WIDTH 30)
(define BAR-COLOR "lightblue")


;;==================
;; Data definitions:

(define-struct school (name tuition next))
;; School is one of:
;;  - false
;;  - (make-school String Natural School)
;; interp. an arbitrary number of schools, where for each school we have its
;;         name and its tuition in USD

(define S1 false)
(define S2 (make-school "School1" 27797
                        (make-school "School2" 23300
                                     (make-school "School3" 28500 false))))
#;
(define (fn-for-school s)
  (cond [(false? school) (...)]
        [else
         (... (school-name s)
              (school-tuition s)
              (fn-for-los (school-next s)))]))

; Template rules used:
;  - one of: 2 cases
;  - atomic distinct: false
;  - compound: (make-school String Natural School)
;  - atomic non-distinct: (school-name s) is String
;  - atomic non-distinct: (school-tuition s) is Natural
;  - self-reference: (school-next s) is School


;;==================
;; Functions:

; School -> Image
; produce a bar chart showing the name and tuition of consumed schools
(check-expect (chart false) (square 0 "solid" "white"))
(check-expect (chart (make-school "School1" 27797 false))
              (beside/align "bottom"
                            (overlay/align "center" "bottom"
                                           (rotate 90 (text "School1" FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* 27797 Y-SCALE) "outline" "black")
                                           (rectangle BAR-WIDTH (* 27797 Y-SCALE) "solid" BAR-COLOR))
                            (square 0 "solid" "white")))
(check-expect (chart (make-school "School1" 27797
                                        (make-school "School2" 23300
                                                     (make-school "School3" 28500 false))))
              (beside/align "bottom"
                            (overlay/align "center" "bottom"
                                           (rotate 90 (text "School1" FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* 27797 Y-SCALE) "outline" "black")
                                           (rectangle BAR-WIDTH (* 27797 Y-SCALE) "solid" BAR-COLOR))
                            (overlay/align "center" "bottom"
                                           (rotate 90 (text "School2" FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* 23300 Y-SCALE) "outline" "black")
                                           (rectangle BAR-WIDTH (* 23300 Y-SCALE) "solid" BAR-COLOR))
                            (overlay/align "center" "bottom"
                                           (rotate 90 (text "School3" FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* 28500 Y-SCALE) "outline" "black")
                                           (rectangle BAR-WIDTH (* 28500 Y-SCALE) "solid" BAR-COLOR))
                            (square 0 "solid" "white")))

;(define (chart s) (square 0 "solid" "white"))   ;stub

(define (chart s)
  (cond [(false? s) (square 0 "solid" "white")]
        [else
         (beside/align "bottom"
                       (overlay/align "center" "bottom"
                                           (rotate 90 (text (school-name s) FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* (school-tuition s) Y-SCALE) "outline" "black")
                                           (rectangle BAR-WIDTH (* (school-tuition s) Y-SCALE) "solid" BAR-COLOR))
                       (chart (school-next s)))]))

