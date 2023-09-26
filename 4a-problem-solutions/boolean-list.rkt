;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname boolean-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;============================================================
;; boolean-list-starter.rkt
;;============================================================
;; Data definitions:

; ListOfBoolean is one of:
;  - empty
;  - (cons Boolean ListOfBoolean)
; interp. boolean in a list
(define LOB1 empty)
(define LOB2 (cons false (cons true empty)))
#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (first lob)
              (fn-for-lob (rest lob)))]))

; Template rules used:
;  - one of: 2 cases
;  - atomic distinct: empty
;  - compound: (cons Number ListOfBoolean)
;  - self-reference: (rest lob) is ListOfBoolean


;; =================
;; Functions:

; ListOfBoolean -> Boolean
; produce true if all values in the list are true (including empty)
(check-expect (all-true? empty) true)
(check-expect (all-true? (cons true (cons true empty))) true)
(check-expect (all-true? (cons true (cons false empty))) false)

;(define (all-true? lob) true)    ;stub

(define (all-true? lob)
  (cond [(empty? lob) true]
        [else
         (and (first lob)
              (all-true? (rest lob)))]))
