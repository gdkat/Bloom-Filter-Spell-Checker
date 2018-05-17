; *********************************************
; *  314 Principles of Programming Languages  *
; *  Spring 2018                              *
; *  Student Version                          *
; *********************************************
#lang racket
;; contains "ctv", "A", and "reduce" definitions
(require "include.rkt")

;; contains dictionary definition
(require "test-dictionary.rkt")
;(require "dictionary.rkt")

;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; *** CODE FOR ANY HELPER FUNCTION GOES HERE ***

;calculate key of word (given in reverse by key function to adhere to left to right criteria)
(define l2rkey
  (lambda (w)
    (if (null? w) 0
        (+ (* 31 (l2rkey (cdr w))) (ctv (car w)) ) )
))

;generate list of lists holding hash values for words in dictionary per hash function
(define dictVectors
  (lambda (hashfl dict)
    (map (lambda (hf) (map hf dict)) hashfl )
))

;generate list of hash values for given word per hash function
(define wordVector
  (lambda (hashfl w)
    (map (lambda (hf) (hf w)) hashfl)
))

;generate bitvector representation list per hashfunction and test to see if hash value exists in all bitvector representations
(define inDict
  (lambda (wv dv)
    (let ((bv (map inVector wv dv)))
      (test-bvr bv)
)))

;generate bitvector representation '((num hash occurences) (num mismatches)) for one hash function
(define inVector
  (lambda (hv dhl)
    (reduce (hshchk hv) dhl '(0 0))
))

;increment bitvector representation for match or mismatch
(define hshchk
  (lambda (hv)
     (lambda (num existbv)
       (if (= hv num) (cons (+ (car existbv) 1) (cdr existbv)) (cons (car existbv) (cons (+ (cadr existbv) 1) '())))
       ;(if (> exist 0) (if (= hv num) (+ exist 1) (exist)) (if (= hv num) 1 0)) lambda hv lambda num exist
)))

;see if hash value exists in all bitvector representations
(define test-bvr
  (lambda (bvr)
    ( cond ( (null? bvr) #t )
           ( (> (car (car bvr)) 0) (test-bvr (cdr bvr)))
           ( else #f )
)))
;; -----------------------------------------------------
;; KEY FUNCTION

(define key
  (lambda (w)
      (l2rkey (reverse w))
))

;; -----------------------------------------------------
;; HASH FUNCTION GENERATORS

;; value of parameter "size" should be a prime number
(define gen-hash-division-method
  (lambda (size) ;; range of values: 0..size-1
     (lambda (w)
       (modulo (key w) size)
)))

;; value of parameter "size" is not critical
;; Note: hash functions may return integer values in "real"
;;       format, e.g., 17.0 for 17

(define gen-hash-multiplication-method
  (lambda (size) ;; range of values: 0..size-1
     (lambda (w)
       (floor (* size (- (* (key w) A) (floor (* (key w) A))) ))
)))


;; -----------------------------------------------------
;; EXAMPLE HASH FUNCTIONS AND HASH FUNCTION LISTS

(define hash-1 (gen-hash-division-method 701))
(define hash-2 (gen-hash-division-method 899))
(define hash-3 (gen-hash-multiplication-method 700))
(define hash-4 (gen-hash-multiplication-method 900))

(define hashfl-1 (list hash-1 hash-2 hash-3 hash-4))
(define hashfl-2 (list hash-1 hash-3))

;; -----------------------------------------------------
;; EXAMPLE HASH VALUES
;;   to test your hash function implementation
;;
;;  (hash-1 '(h e l l o)) ;==> 674
;;  (hash-1 '(d a y))     ;==> 395
;;  (hash-1 '(c l a s s)) ;==> 360

;;  (hash-2 '(h e l l o)) ;==> 139
;;  (hash-2 '(d a y))     ;==> 304
;;  (hash-2 '(c l a s s)) ;==> 205
;;
;;  (hash-3 '(h e l l o)) ;==> 552.0
;;  (hash-3 '(d a y))     ;==> 501.0
;;  (hash-3 '(c l a s s)) ;==> 247.0
;;
;;  (hash-4 '(h e l l o)) ;==> 710.0
;;  (hash-4 '(d a y))     ;==> 644.0
;;  (hash-4 '(c l a s s)) ;==> 317.0

;; -----------------------------------------------------
;; SPELL CHECKER GENERATOR

(define gen-checker
  (lambda (hashfunctionlist dict)
    (let ((dictvects (dictVectors hashfunctionlist dict)))
     (lambda (w)
       (let ((wordvect (wordVector hashfunctionlist w)))
         (inDict wordvect dictvects)
)))))


;; -----------------------------------------------------
;; EXAMPLE SPELL CHECKERS

(define checker-1 (gen-checker hashfl-1 dictionary))
(define checker-2 (gen-checker hashfl-2 dictionary))

;; EXAMPLE APPLICATIONS OF A SPELL CHECKER
;;
;;  (checker-1 '(a r g g g)) ;==> #f
;;  (checker-2 '(h e l l o)) ;==> #t
;;  (checker-1 '(s))