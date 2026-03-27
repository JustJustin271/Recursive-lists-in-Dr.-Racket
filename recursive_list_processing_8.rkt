#lang racket
(require rackunit)

;; merge : List-of-Numbers List-of-Numbers -> List-of-Numbers
;; Given two sorted lists, produces a single sorted list
;; containing all elements of both.
;; PRECONDITION: both lst1 and lst2 are sorted in ascending order.
(define (merge l1 l2)
  (cond
   [(empty? l1)  l2]
   [(empty? l2)  l1]
   [(< (first l1) (first l2))
    (cons (first l1)
          (merge (cdr l1) l2))]
   [else
    (cons (first l2)
          (merge l1 (cdr l2)))]))

(check-equal? (merge '() '()) '())
(check-equal? (merge '(1 3 5) '()) '(1 3 5))
(check-equal? (merge '() '(2 4 6)) '(2 4 6))
(check-equal? (merge '(1 3 5) '(2 4 6)) '(1 2 3 4 5 6))
(check-equal? (merge '(1 1 3) '(1 2 4)) '(1 1 1 2 3 4))

;; every-other : List-of-any -> List-of-any
;; Takes in a list and outputs everyother part of the list
(define (every-other lst1)
  (cond
   [(empty? lst1) empty]
   [(empty? (cdr lst1)) lst1]
   [else (cons (car lst1) (every-other (cddr lst1)))]))

(check-equal? (every-other '()) '())
(check-equal? (every-other '(a)) '(a))
(check-equal? (every-other '(a b)) '(a))
(check-equal? (every-other '(a b c)) '(a c))
(check-equal? (every-other '(a b c d e)) '(a c e))
(check-equal? (every-other '(1 2 3 4 5 6)) '(1 3 5))

;; zip : List List -> List-of-Lists
;; Takes in a list and merges the 2 lists other,
;; But only every other element
;; Precondition: Both lists have the same length.
(define(zip l1 l2)
  (cond
   [(and (empty? l2) (empty? l1)) empty]
   
   [else (cons (list (car l1) (car l2)) (zip (cdr l1) (cdr l2)))]))

(check-equal? (zip '() '()) '())
(check-equal? (zip '(1 2 3) '(a b c)) '((1 a) (2 b) (3 c)))
(check-equal? (zip '(x) '(y)) '((x y)))

;------run-length-encode helper functions

;; help-count : any list-of-any -> num
;; counts the number of repetitions of an element
;; in a given list
(define (help-count element lst)
  (cond
    [(empty? lst) 0]
    [(not (equal? (car lst) element)) 0]
    [else (add1 (help-count element (cdr lst)))]))

(check-equal? (help-count 'a '()) 0)
(check-equal? (help-count 'a '(a a b c)) 2)
(check-equal? (help-count 1 '(1 1 1 1 1 2 3 4 1)) 5)

;; drop-repeat : any list-of-any -> list-of-any
;; Takes in a list and drops the repeated elements at the beginning of the list
(define (drop-repeat element lst)
  (cond
    [(empty? lst) empty]
    [(not (equal? (car lst) element)) lst]
    [else (drop-repeat element (cdr lst))]))

(check-equal? (drop-repeat 'a '(a a b c)) '(b c))
(check-equal? (drop-repeat 1 '() )'())

;------------------------------------

;; run-length-encode : List-of-any -> List-of-lists-of-any & nums
;; Takes in a list and outputs the element and the number of times that element was repeated consecutively
(define (run-length-encode lst)
  (cond
    [(empty? lst) empty]
    [else (cons
           (list (first lst) (help-count (first lst) lst))
           (run-length-encode (drop-repeat (first lst) lst)))]))


(check-equal? (run-length-encode '(a a a b b c))
              '((a 3) (b 2) (c 1)))
(check-equal? (run-length-encode '(x x y x x))
              '((x 2) (y 1) (x 2)))
(check-equal? (run-length-encode '()) '())
(check-equal? (run-length-encode '(a)) '((a 1)))

;;--------my-reverse helper functions

(define (rev-help so-far remaining)
  (cond
    [(empty? remaining) so-far]
    [else (rev-help (cons (first remaining) so-far) (rest remaining))]))

(check-equal? (rev-help empty (list 3 2 1)) '(1 2 3))

;;-------------------------

;; my-reverse : list-of-any -> list-of-any
;; Reverses the order of a given list
;; From now last to first w/o using the built-in function
(define (my-reverse lst)
  (rev-help '() lst))

(check-equal? (my-reverse (list 1 2 3)) (list 3 2 1))
(check-equal? (my-reverse '()) '())
(check-equal? (my-reverse '(1)) '(1))
(check-equal? (my-reverse '(1 2 3)) '(3 2 1))
(check-equal? (my-reverse '(a b c d)) '(d c b a))

;; atom : any -> bool
;; returns true if x is not a pair
(define (atom? x)
  (not (pair? x)))


;; flatten : list-of-any -> list-of-any
;; Removes all nested lists within the function
;; And produces a flat list of all of the atoms in the same order
(define (flatten lst)
  (cond
    [(empty? lst) empty]
    [(atom? (car lst)) (cons (car lst) (flatten (cdr lst)))]
    [else (append (flatten (car lst)) (flatten (cdr lst)))]))

(check-equal? (flatten '()) '())
(check-equal? (flatten '(1 2 3)) '(1 2 3))
(check-equal? (flatten '(1 (2 3) 4)) '(1 2 3 4))
(check-equal? (flatten '((1 (2)) (3 (4 (5))))) '(1 2 3 4 5))


#|
March 27th, 2026
Salutations there for those are reading, whether you're human or not
is a separate question, now, hope you enjoy the function of lists and
I hope they are accurate, they are only accurate to my current knowledge as of 5:27pm EDT :)
|#