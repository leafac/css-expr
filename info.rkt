#lang info

(define collection "css-expr")
(define version "0.0.1")
(define deps '("base" "rackunit-lib" "typed-racket-lib" "typed-racket-more"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("documentation/css-expr.scrbl" ())))
(define compile-omit-paths '("tests"))
(define pkg-desc "S-expression-based CSS")
(define pkg-authors '(leafac))
