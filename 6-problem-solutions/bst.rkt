;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname bst) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;;============================================================
;; bst
;;============================================================
;; Constant:
(define TEXT-SIZE 14)
(define TEXT-COLOR "black")

(define KEY-VAL-SEPARATOR ":")

(define VSPACE (rectangle 1 10 "solid" "white"))
(define HSPACE (rectangle 10 1 "solid" "white"))

(define MTTREE (rectangle 20 10 "solid" "white"))

;; =================
;; Data definitions:

(define-struct node (key val l r))

;; BST (Binary Search Tree) is one of:
;;  - false
;;  - (make-node Integer String BST BST)
;; interp. false means no BST, or empty BST
;;         key is the node key
;;         val is the node val
;;         l and r are left and right subtrees
;; INVARIANT: for a given node:
;;     key is > all keys in its l(eft) child
;;     key is < all keys in its r(ight) child
;;     the same key never appears twice in the tree
(define BST0 false)
(define BST1 (make-node 1 "abc" false false))
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define BST3 (make-node 3 "ilk" BST1 BST4))
(define BST42
  (make-node 42 "ily"
             (make-node 27 "wit" (make-node 14 "olp" false false) false)
             (make-node 50 "dug" false false)))
(define BST10
  (make-node 10 "why" BST3 BST42))

(define (fn-for-bst t)
  (cond [(false? t) (...)] 
        [else
         (... (node-key t)
              (node-val t)
              (fn-for-bst (node-l t))
              (fn-for-bst (node-r t)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: false
;;  - compound: (make-node Integer String BST BST)


;; =================
;; Functions:

;; BST Natural -> String or false
;; Try to find node with given key, if found produce value; if not found produce false.
(check-expect (lookup-key BST0  99) false)
(check-expect (lookup-key BST1   1) "abc")
(check-expect (lookup-key BST1   0) false)
(check-expect (lookup-key BST1  99) false)
(check-expect (lookup-key BST10  1) "abc")
(check-expect (lookup-key BST10  4) "dcj")
(check-expect (lookup-key BST10 27) "wit")
(check-expect (lookup-key BST10 50) "dug")

;(define (lookup-key t k) "")  ;stub

(define (lookup-key t k)
  (cond [(false? t) false] 
        [else
         (cond [(= k (node-key t)) (node-val t)]
               [(< k (node-key t))
                (lookup-key (node-l t) k)]
               [(> k (node-key t))
                (lookup-key (node-r t) k)])]))


;; BST -> Image
;; produce a simple rendering of the tree
(check-expect (render-bst false) MTTREE)
(check-expect (render-bst BST1) (above (text (string-append "1" KEY-VAL-SEPARATOR "abc")
                                             TEXT-SIZE
                                             TEXT-COLOR)
                                       VSPACE
                                       (beside (render-bst false)
                                               HSPACE
                                               (render-bst false))))
(check-expect (render-bst BST4) (above (text (string-append "4" KEY-VAL-SEPARATOR "dcj")
                                             TEXT-SIZE
                                             TEXT-COLOR)
                                       VSPACE
                                       (beside (render-bst false)
                                               HSPACE
                                               (render-bst (make-node 7 "ruf" false false)))))
(check-expect (render-bst BST3) (above (text (string-append "3" KEY-VAL-SEPARATOR "ilk")
                                             TEXT-SIZE
                                             TEXT-COLOR)
                                       VSPACE
                                       (beside (render-bst BST1)
                                               HSPACE
                                               (render-bst BST4))))

;(define (render-bst t) (square 0 "solid" "white"))  ;stub

(define (render-bst t)
  (cond [(false? t) MTTREE] 
        [else
         (above (text (string-append (number->string (node-key t)) KEY-VAL-SEPARATOR (node-val t))
                      TEXT-SIZE
                      TEXT-COLOR)
                VSPACE
                (beside (render-bst (node-l t))
                        HSPACE
                        (render-bst (node-r t))))]))

