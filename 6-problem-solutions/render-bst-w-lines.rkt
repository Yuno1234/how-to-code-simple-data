;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname render-bst-w-lines-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;;============================================================
;; render-bst-w-lines
;;============================================================
;; Constants

(define TEXT-SIZE  14)
(define TEXT-COLOR "BLACK")

(define KEY-VAL-SEPARATOR ":")

(define MTTREE (rectangle 20 1 "solid" "white"))


;; =================
;; Data definitions:

(define-struct node (key val l r))
;; A BST (Binary Search Tree) is one of:
;;  - false
;;  - (make-node Integer String BST BST)
;; interp. false means no BST, or empty BST
;;         key is the node key
;;         val is the node val
;;         l and r are left and right subtrees
;; INVARIANT: for a given node:
;;     key is > all keys in its l(eft)  child
;;     key is < all keys in its r(ight) child
;;     the same key never appears twice in the tree

(define BST0 false)
(define BST1 (make-node 1 "abc" false false))
(define BST7 (make-node 7 "ruf" false false)) 
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define BST3 (make-node 3 "ilk" BST1 BST4))
(define BST42 
  (make-node 42 "ily"
             (make-node 27 "wit" (make-node 14 "olp" false false) false)
             (make-node 50 "dug" false false)))
(define BST10
  (make-node 10 "why" BST3 BST42))
(define BST100 
  (make-node 100 "large" BST10 false))
#;
(define (fn-for-bst t)
  (cond [(false? t) (...)]
        [else
         (... (node-key t)    ;Integer
              (node-val t)    ;String
              (fn-for-bst (node-l t))
              (fn-for-bst (node-r t)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic-distinct: false
;;  - compound: (make-node Integer String BST BST)
;;  - self reference: (node-l t) has type BST
;;  - self reference: (node-r t) has type BST


;; =================
;; Functions:

;; BST -> Image
;; produce a simple rendering of the tree with lines between the nodes
(check-expect (render-bst false) MTTREE)
(check-expect (render-bst BST1)
              (above (render-key-val 1 "abc") 
                     (lines (image-width (render-bst false))
                            (image-width (render-bst false)))
                     (beside (render-bst false)
                             (render-bst false))))


;(define (render-bst bst) (square 0 "solid" "white"))  ;stub

(define (render-bst bst)
  (cond [(false? bst) MTTREE]
        [else
         (above (render-key-val (node-key bst) (node-val bst))
                (lines (image-width (render-bst (node-l bst)))
                       (image-width (render-bst (node-r bst))))
                (beside (render-bst (node-l bst))
                        (render-bst (node-r bst))))]))


;; Integer String -> Image
;; produce a text image with consumed key and value
(check-expect (render-key-val 1 "abc")
              (text (string-append (number->string 1) KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))

(define (render-key-val key val)
  (text (string-append (number->string key) KEY-VAL-SEPARATOR val) TEXT-SIZE TEXT-COLOR))


;; Integer Integer -> Image
;; produce lines to l/r subtrees based on width of those subtrees
(check-expect (lines 60 130)
              (add-line (add-line (rectangle (+ 60 130) (/ (+ 60 130) 4) "solid" "white")
                                  (/ (+ 60 130) 2) 0
                                  (/ 60 2) (/ (+ 60 130) 4)
                                  "black")
                        (/ (+ 60 130) 2) 0
                        (+ 60 (/ 130 2)) (/ (+ 60 130) 4)
                        "black"))

(define (lines lw lh)
  (add-line (add-line (rectangle (+ lw lh) (/ (+ lw lh) 4) "solid" "white")
                                  (/ (+ lw lh) 2) 0
                                  (/ lw 2) (/ (+ lw lh) 4)
                                  "black")
                        (/ (+ lw lh) 2) 0
                        (+ lw (/ lh 2)) (/ (+ lw lh) 4)
                        "black"))
              