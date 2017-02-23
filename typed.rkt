#lang typed/racket/base
(require syntax/parse/define)

(require/typed/provide "main.rkt" [css-expr->css (-> Sexp String)])

(define-simple-macro (css-expr any ...) `(any ...))