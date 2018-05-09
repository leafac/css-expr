#lang info

(define collection "css-expr")
(define version "0.0.2")
(define deps '("base" "rackunit-lib" "typed-racket-lib" "typed-racket-more" "nanopass"))
(define build-deps '("scribble-lib" "racket-doc" "sandbox-lib"))
(define scribblings '(("css-expr.scrbl" ())))
(define pkg-desc "S-expression-based CSS")
(define pkg-authors '(leafac))
