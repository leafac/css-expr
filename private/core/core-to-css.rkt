#lang typed/racket/base
(require "abstract-syntax-tree.rkt"
         racket/match racket/string racket/format racket/list)

(provide Core->CSS CSS)

(define-type CSS String)

(: Stylesheet->CSS (-> Stylesheet CSS))
(define (Core->CSS stylesheet)
  (Stylesheet->CSS stylesheet))

(: Stylesheet->CSS (-> Stylesheet CSS))
(define/match (Stylesheet->CSS stylesheet)
  [((Stylesheet rules)) (apply string-append (map Rule->CSS rules))])

(: Rule->CSS (-> Rule CSS))
(define/match (Rule->CSS rule)
  [((? QualifiedRule?)) (QualifiedRule->CSS rule)]
  [((? AtRule?)) (AtRule->CSS rule)])

(: QualifiedRule->CSS (-> QualifiedRule CSS))
(define/match (QualifiedRule->CSS qualified-rule)
  [((QualifiedRule selectors declarations))
   (~a
    (string-join (map Selector->CSS selectors) ",")
    "{"
    (apply string-append
           (for/list : (Listof CSS) ([declaration declarations])
             (~a (Declaration->CSS declaration) ";")))
    "}")])

(: AtRule->CSS (-> AtRule CSS))
(define/match (AtRule->CSS at-rule)
  [((AtRule name expressions elements))
   (~a "@" name (if (empty? expressions) "" " ")
       (string-join (map AtRule-Expression->CSS expressions) ",")
       (if elements
           (~a "{"
               (apply string-append
                      (for/list : (Listof CSS) ([element elements])
                        (match element
                          [(? QualifiedRule?) (QualifiedRule->CSS element)]
                          [(? Declaration?) (~a (Declaration->CSS element) ";")])))
               "}")
           ";"))])

(: AtRule-Expression->CSS (-> AtRule-Expression CSS))
(define/match (AtRule-Expression->CSS at-rule/expression)
  [((? AtRule-Expression-Declaration?)) (AtRule-Expression-Declaration->CSS at-rule/expression)]
  [((? AtRule-Expression-Value?)) (AtRule-Expression-Value->CSS at-rule/expression)]
  [((? AtRule-Expression-Operation?)) (AtRule-Expression-Operation->CSS at-rule/expression)])

(: AtRule-Expression-Declaration->CSS (-> AtRule-Expression-Declaration CSS))
(define/match (AtRule-Expression-Declaration->CSS at-rule/expression/declaration)
  [((AtRule-Expression-Declaration declaration)) (~a "(" (Declaration->CSS declaration) ")")])

(: AtRule-Expression-Value->CSS (-> AtRule-Expression-Value CSS))
(define/match (AtRule-Expression-Value->CSS at-rule/expression/value)
  [((AtRule-Expression-Value value)) (Value->CSS value)])

(: AtRule-Expression-Operation->CSS (-> AtRule-Expression-Operation CSS))
(define/match (AtRule-Expression-Operation->CSS at-rule/expression/operation)
  [((? AtRule-Expression-Operation-Unary?))
   (AtRule-Expression-Operation-Unary->CSS at-rule/expression/operation)]
  [((? AtRule-Expression-Operation-NAry?))
   (AtRule-Expression-Operation-NAry->CSS at-rule/expression/operation)])

(: AtRule-Expression-Operation-Unary->CSS (-> AtRule-Expression-Operation-Unary CSS))
(define/match (AtRule-Expression-Operation-Unary->CSS at-rule/expression/operation/unary)
  [((AtRule-Expression-Operation-Unary operator operand))
   (~a operator " " (AtRule-Expression->CSS operand))])

(: AtRule-Expression-Operation-NAry->CSS (-> AtRule-Expression-Operation-NAry CSS))
(define/match (AtRule-Expression-Operation-NAry->CSS at-rule/expression/operation/NAry)
  [((AtRule-Expression-Operation-NAry operator operands))
   (string-join (map AtRule-Expression->CSS operands)
                (if (equal? operator '| |) (~a operator) (~a " " operator " " )))])

(: Declaration->CSS (-> Declaration CSS))
(define/match (Declaration->CSS declaration)
  [((Declaration name values important?))
   (~a name ":" (string-join (map Value->CSS values) ",") (if important? " !important" ""))])

(: Value->CSS (-> Value CSS))
(define/match (Value->CSS value)
  [((? Value-Literal?)) (Value-Literal->CSS value)]
  [((? Value-Measurement?)) (Value-Measurement->CSS value)]
  [((? Value-FunctionApplication?)) (Value-FunctionApplication->CSS value)]
  [((? Value-Operation?)) (Value-Operation->CSS value)])

(: Value-Literal->CSS (-> Value-Literal CSS))
(define/match (Value-Literal->CSS value/literal)
  [((Value-Literal (? symbol? literal))) (~a literal)]
  [((Value-Literal (? number? literal))) (~a literal)]
  [((Value-Literal (? string? literal))) (~a "\"" literal "\"")])

(: Value-Measurement->CSS (-> Value-Measurement CSS))
(define/match (Value-Measurement->CSS value/measurement)
  [((Value-Measurement unit magnitude)) (~a magnitude unit)])

(: Value-FunctionApplication->CSS (-> Value-FunctionApplication CSS))
(define/match (Value-FunctionApplication->CSS value/function-application)
  [((Value-FunctionApplication function arguments))
   (~a function "(" (string-join (map Value->CSS arguments) ",") ")")])

(: Value-Operation->CSS (-> Value-Operation CSS))
(define/match (Value-Operation->CSS value/operation)
  [((Value-Operation operator operands))
   (string-join (map Value->CSS operands)
                (if (member operator '(+ -)) (~a " " operator " ") (~a operator)))])

(: Selector->CSS (-> Selector CSS))
(define/match (Selector->CSS selector)
  [((? Selector-Tag?)) (Selector-Tag->CSS selector)]
  [((? Selector-Namespaced?)) (Selector-Namespaced->CSS selector)]
  [((? Selector-Combination?)) (Selector-Combination->CSS selector)]
  [((? Selector-Prefixed?)) (Selector-Prefixed->CSS selector)]
  [((? Selector-Attribute?)) (Selector-Attribute->CSS selector)])

(: Selector-Tag->CSS (-> Selector-Tag CSS))
(define/match (Selector-Tag->CSS selector/tag)
  [((Selector-Tag tag)) (~a tag)])

(: Selector-Namespaced->CSS (-> Selector-Namespaced CSS))
(define/match (Selector-Namespaced->CSS selector/namespaced)
  [((Selector-Namespaced namespace name)) (~a (or namespace "") "|" name)])

(: Selector-Combination->CSS (-> Selector-Combination CSS))
(define/match (Selector-Combination->CSS selector/combination)
  [((Selector-Combination combinator combinands))
   (string-join (map Selector->CSS combinands)
                (if (Selector-Combination-Combinator-Reference? combinator)
                    (Selector-Combination-Combinator-Reference->CSS combinator)
                    (~a combinator)))])

(: Selector-Combination-Combinator-Reference->CSS
   (-> Selector-Combination-Combinator-Reference CSS))
(define/match (Selector-Combination-Combinator-Reference->CSS
               selector/combination/combinator/reference)
  [((Selector-Combination-Combinator-Reference reference)) (~a " /" reference "/ ")])

(: Selector-Prefixed->CSS (-> Selector-Prefixed CSS))
(define/match (Selector-Prefixed->CSS selector/prefixed)
  [((Selector-Prefixed prefix selector subject))
   (~a (if selector (Selector->CSS selector) "")
       prefix
       (if (Selector-FunctionApplication? subject)
           (Selector-FunctionApplication->CSS subject)
           (~a subject)))])

(: Selector-FunctionApplication->CSS (-> Selector-FunctionApplication CSS))
(define/match (Selector-FunctionApplication->CSS selector/function-application)
  [((Selector-FunctionApplication function arguments))
   (~a function "("
       (string-join
        (for/list : (Listof CSS) ([argument arguments])
          (match argument
            [(? Selector?) (Selector->CSS argument)]
            [(? Selector-FunctionApplication?)
             (Selector-FunctionApplication->CSS argument)]
            [(? Selector-An+B?) (Selector-An+B->CSS argument)]))
        ",") ")")])

(: Selector-Attribute->CSS (-> Selector-Attribute CSS))
(define/match (Selector-Attribute->CSS selector/attribute)
  [((Selector-Attribute selector subject))
   (~a (if selector (Selector->CSS selector) "")
       "["
       (match subject
         [(? symbol?) (~a subject)]
         [(? Selector-Namespaced?) (Selector-Namespaced->CSS subject)]
         [(? Selector-Attribute-Operation?)
          (Selector-Attribute-Operation->CSS subject)])
       "]")])

(: Selector-Attribute-Operation->CSS (-> Selector-Attribute-Operation CSS))
(define/match (Selector-Attribute-Operation->CSS selector/attribute/operation)
  [((Selector-Attribute-Operation operator `(,operand/left . ,operand/right)))
   (~a (Selector-Attribute-Operation-Operand->CSS operand/left)
       operator
       (Selector-Attribute-Operation-Operand->CSS operand/right))])

(: Selector-Attribute-Operation-Operand->CSS
   (-> Selector-Attribute-Operation-Operand CSS))
(define/match (Selector-Attribute-Operation-Operand->CSS selector/attribute/operation/operand)
  [((? Selector-Attribute-Operation-Operand-Literal?))
   (Selector-Attribute-Operation-Operand-Literal->CSS selector/attribute/operation/operand)]
  [((? Selector-Attribute-Operation-Operand-CaseInsensitive?))
   (Selector-Attribute-Operation-Operand-CaseInsensitive->CSS selector/attribute/operation/operand)])

(: Selector-Attribute-Operation-Operand-Literal->CSS
   (-> Selector-Attribute-Operation-Operand-Literal CSS))
(define/match (Selector-Attribute-Operation-Operand-Literal->CSS
               selector/attribute/operation/operand/literal)
  [((Selector-Attribute-Operation-Operand-Literal (? symbol? literal)))
   (~a literal)]
  [((Selector-Attribute-Operation-Operand-Literal (? string? literal)))
   (~a "\"" literal "\"" )])

(: Selector-Attribute-Operation-Operand-CaseInsensitive->CSS
   (-> Selector-Attribute-Operation-Operand-CaseInsensitive CSS))
(define/match (Selector-Attribute-Operation-Operand-CaseInsensitive->CSS
               selector/attribute/operation/operand/case-insensitive)
  [((Selector-Attribute-Operation-Operand-CaseInsensitive string))
   (~a "\"" string "\" i")])

(: Selector-An+B->CSS (-> Selector-An+B CSS))
(define/match (Selector-An+B->CSS selector/An+B)
  [((? Selector-An+B-Literal?)) (Selector-An+B-Literal->CSS selector/An+B)]
  [((? Selector-An+B-Proper?)) (Selector-An+B-Proper->CSS selector/An+B)])

(: Selector-An+B-Literal->CSS (-> Selector-An+B-Literal CSS))
(define/match (Selector-An+B-Literal->CSS selector/An+B/literal)
  [((Selector-An+B-Literal (? symbol? literal))) (~a literal)]
  [((Selector-An+B-Literal (? integer? literal))) (~a literal)])

(: Selector-An+B-Proper->CSS (-> Selector-An+B-Proper CSS))
(define/match (Selector-An+B-Proper->CSS expression)
  [((Selector-An+B-Proper step offset)) (~a (or step "") "n" (if offset (~a "+" offset) ""))])
