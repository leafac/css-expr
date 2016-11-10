#lang typed/racket/base
(require racket/match racket/list racket/string)

(provide tokenize)

(: tokenize (-> Sexp Sexp))
(define (tokenize expression)
  (: tokenize* (-> Sexp (Listof Sexp)))
  (define/match (tokenize* expression)
    [((? symbol?))
     (define identifier/string (symbol->string expression))
     (define identifier/string/length (string-length identifier/string))
     (define identifier/multiple-characters? (> identifier/string/length 1))
     (define identifier/starts-with-@? (string-prefix? identifier/string "@"))
     (define separate-@-prefix? (and identifier/multiple-characters? identifier/starts-with-@?))
     (define identifier/index/start (if separate-@-prefix? 1 0))
     (define identifier/transformed
       (string->symbol (substring identifier/string identifier/index/start)))
     `(,@(if separate-@-prefix? '(@) '()) ,identifier/transformed)]
    [((? list?)) `(,((inst append-map Sexp Sexp) tokenize* expression))]
    [(_) `(,expression)])
  
  (first (tokenize* expression)))