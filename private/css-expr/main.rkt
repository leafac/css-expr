#lang racket/base
(require syntax/parse/define)

(provide css-expr)

(define-simple-macro (css-expr any ...) `(any ...))
