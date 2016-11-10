#lang typed/racket/base
(require "private/css-expr/typed.rkt"
         "private/css-expr/lexer.rkt"
         "private/css-expr/parser.rkt"
         "private/extended/extended-to-core.rkt"
         "private/core/core-to-css.rkt")

(provide css-expr css-expr->css)

(: css-expr->css (-> Sexp String))
(define (css-expr->css css-expr)
  (Core->CSS (Extended->Core (parse (tokenize css-expr)))))
