#lang typed/racket/base
(require (prefix-in Extended: "abstract-syntax-tree.rkt")
         "../core/abstract-syntax-tree.rkt"
         racket/match racket/list racket/format racket/function)

(provide Extended->Core)

(: Extended->Core (-> Stylesheet Stylesheet))
(define/match (Extended->Core stylesheet)
  [((Stylesheet rules)) (Stylesheet (append-map flatten rules))])

;; ---------------------------------------------------------------------------------------------------

(: flatten (-> Rule (Listof Rule)))
(define (flatten rule)
  (flatten/Rule rule))

(: flatten/Rule (-> Rule (Listof Rule)))
(define/match (flatten/Rule rule)
  [((? Extended:QualifiedRule?)) (flatten/QualifiedRule rule)]
  [((? Extended:AtRule?)) (flatten/AtRule rule)])

(: flatten/QualifiedRule (-> Extended:QualifiedRule (Listof Rule)))
(define/match (flatten/QualifiedRule qualified-rule)
  [((Extended:QualifiedRule selectors elements))
   (define selectors/core
     (for/list : (Listof Selector) ([selector selectors])
       (match selector
         [(? Extended:Selector-Parent?)
          (raise-user-error
           (~a "Extended->Core: reference to parent found at top level;\n"
               " only nested rules can refer to parents\n"
               "  qualified-rule: " qualified-rule "\n"
               "  selector/parent: " selector))]
         [(? Selector?) selector])))
   (define declarations (filter Extended:Declaration? elements))
   (define qualified-rules (filter Extended:QualifiedRule? elements))
   (define at-rules (filter Extended:AtRule? elements))
   (define declarations/flattened (append-map flatten/Declaration declarations))
   (define qualified-rules/flattened
     (append-map
      flatten/QualifiedRule
      (for/list : (Listof Extended:QualifiedRule) ([qualified-rule qualified-rules])
        (match-define (Extended:QualifiedRule selectors/nested elements/nested) qualified-rule)
        (Extended:QualifiedRule (merge/Selectors selectors/core selectors/nested) elements/nested))))
   (define at-rules/flattened
     (append-map
      flatten/AtRule
      (for/list : (Listof Extended:AtRule) ([at-rule at-rules])
        (match at-rule
          [(Extended:AtRule name expressions #f) at-rule]
          [(Extended:AtRule name expressions (? list? elements/nested))
           (Extended:AtRule
            name expressions
            `(,(Extended:QualifiedRule selectors/core elements/nested)))]))))
   `(,@(if (empty? declarations/flattened)
           '()
           `(,(QualifiedRule selectors/core declarations/flattened)))
     ,@qualified-rules/flattened
     ,@at-rules/flattened)])

(: flatten/AtRule (-> Extended:AtRule (Listof AtRule)))
(define/match (flatten/AtRule at-rule)
  [((Extended:AtRule name expressions #f))
   `(,(AtRule name (map flatten/AtRule-Expression expressions) #f))]
  [((Extended:AtRule name expressions (? list? elements)))
   (define expressions/flattened (map flatten/AtRule-Expression expressions))
   (define declarations (filter Extended:Declaration? elements))
   (define qualified-rules (filter Extended:QualifiedRule? elements))
   (define at-rules (filter Extended:AtRule? elements))
   (define declarations/flattened (append-map flatten/Declaration declarations))
   (define qualified-rules/flattened (append-map flatten/QualifiedRule qualified-rules))
   (define qualified-rules/flattened/qualified-rules
     (filter QualifiedRule? qualified-rules/flattened))
   (define qualified-rules/flattened/at-rules (filter AtRule? qualified-rules/flattened))
   (define at-rules/flattened (append-map flatten/AtRule at-rules))
   (define at-rules/combined (append at-rules/flattened qualified-rules/flattened/at-rules))
   (define at-rules/merged
     (for/list : (Listof AtRule) ([at-rule at-rules/combined])
       (match at-rule
         [(AtRule name/nested _ _) #:when (not (equal? name name/nested)) at-rule]
         [(AtRule _ expressions/nested #f)
          (AtRule name (merge/AtRule-Expressions expressions/flattened expressions/nested) #f)]
         [(AtRule _ expressions/nested (? list? elements/nested))
          (AtRule name (merge/AtRule-Expressions expressions/flattened expressions/nested)
                  elements/nested)])))
   (define elements/nested/flattened
     (append declarations/flattened qualified-rules/flattened/qualified-rules))
   `(,@(if (empty? elements/nested/flattened)
           '()
           `(,(AtRule name expressions/flattened elements/nested/flattened)))
     ,@at-rules/merged)])

(: flatten/AtRule-Expression (-> AtRule-Expression AtRule-Expression))
(define/match (flatten/AtRule-Expression at-rule/expression)
  [((AtRule-Expression-Declaration declaration))
   (define declarations/flattened (flatten/Declaration declaration))
   (if (= (length declarations/flattened) 1)
       (AtRule-Expression-Declaration (first declarations/flattened))
       (AtRule-Expression-Operation-NAry
        'and (map AtRule-Expression-Declaration declarations/flattened)))]
  [((? AtRule-Expression-Value?)) at-rule/expression]
  [((AtRule-Expression-Operation-Unary operator operand))
   (AtRule-Expression-Operation-Unary operator (flatten/AtRule-Expression operand))]
  [((AtRule-Expression-Operation-NAry operator operands))
   (AtRule-Expression-Operation-NAry operator (map flatten/AtRule-Expression operands))])

(: flatten/Declaration (-> Declaration (Listof Declaration)))
(define/match (flatten/Declaration declaration)
  [((Extended:Declaration name values important? #f))
   `(,(Declaration name values important?))]
  [((Extended:Declaration name values important? (? list? nested-declarations)))
   (define nested-declarations/flattened (append-map flatten/Declaration nested-declarations))
   (define nested-declarations/merged
     (for/list : (Listof Declaration) ([nested-declaration nested-declarations/flattened])
       (match-define (Declaration name/nested values/nested important?/nested) nested-declaration)
       (Declaration (string->symbol (~a name "-" name/nested)) values/nested important?/nested)))
   `(,@(if (empty? values) '() `(,(Declaration name values important?)))
     ,@nested-declarations/merged)])

;; ---------------------------------------------------------------------------------------------------

(: merge/Selectors (-> (Listof Selector) (Listof Selector) (Listof Selector)))
(define (merge/Selectors selectors/parent selectors/child)
  (for/list ([selectors/pair (cartesian-product selectors/parent selectors/child)])
    (merge/Selector (first selectors/pair) (second selectors/pair))))

(: merge/Selector (-> Selector Selector Selector))
(define (merge/Selector selector/parent selector/child)
  (replace-parent-reference selector/parent (ensure-parent-reference selector/child)))

(: ensure-parent-reference (-> Selector Selector))
(define (ensure-parent-reference selector)
  (if (contains-parent-reference? selector)
      selector
      (Selector-Combination '| | `(,(Extended:Selector-Parent #f) ,selector))))

(: contains-parent-reference? (-> Selector Boolean))
(define (contains-parent-reference? selector)
  (contains-parent-reference?/Selector selector))

(: contains-parent-reference?/Selector (-> Selector Boolean))
(define/match (contains-parent-reference?/Selector selector)
  [((? Selector-Tag?)) #f]
  [((? Selector-Namespaced?)) #f]
  [((Selector-Combination _ combinands)) (ormap contains-parent-reference?/Selector combinands)]
  [((Selector-Prefixed _ selector subject))
   (or (and selector (contains-parent-reference?/Selector selector))
       (match subject
         [(? symbol?) #f]
         [(? Selector-FunctionApplication?)
          (contains-parent-reference?/Selector-FunctionApplication subject)]))]
  [((Selector-Attribute selector _))
   (and selector (contains-parent-reference?/Selector selector))]
  [((? Extended:Selector-Parent?)) #t])

(: contains-parent-reference?/Selector-FunctionApplication (-> Selector-FunctionApplication Boolean))
(define/match (contains-parent-reference?/Selector-FunctionApplication selector/function-application)
  [((Selector-FunctionApplication _ arguments))
   (for/or ([argument arguments])
     (match argument
       [(? Selector?) (contains-parent-reference?/Selector argument)]
       [(? Selector-FunctionApplication?)
        (contains-parent-reference?/Selector-FunctionApplication argument)]
       [(? Selector-An+B?) #f]))])

(: replace-parent-reference (-> Selector Selector Selector))
(define (replace-parent-reference selector/parent selector/child)
  (replace-parent-reference/Selector selector/parent selector/child))

(: replace-parent-reference/Selector (-> Selector Selector Selector))
(define (replace-parent-reference/Selector selector/parent selector/child)
  (match selector/child
    [(Extended:Selector-Parent #f) selector/parent]
    [(Extended:Selector-Parent (? symbol? suffix))
     (match selector/parent
       [(Selector-Tag tag) (Selector-Tag (string->symbol (~a tag "-" suffix)))]
       [(Selector-Namespaced namespace name)
        (Selector-Namespaced namespace (string->symbol (~a name "-" suffix)))]
       [(Selector-Combination combinator combinands)
        (Selector-Combination
         combinator
         `(,@(drop-right combinands 1)
           ,(replace-parent-reference/Selector (last combinands) selector/child)))]
       [(Selector-Prefixed prefix selector (? symbol? subject))
        (Selector-Prefixed prefix selector (string->symbol (~a subject "-" suffix)))]
       [_
        (raise-user-error
         'merge-selectors
         (~a "parent selector `" selector/parent "' is not supported on child selector "
             "`" selector/child "'. See Sass reference for cases that should work: "
             "`http://sass-lang.com/documentation/file.SASS_REFERENCE.html#parent-selector'. "
             "If Sass supports the case and you are seeing this error message, please "
             "report the issue as a bug."))])]
    [(Selector-Combination combinator combinands)
     (Selector-Combination
      combinator (map (curry replace-parent-reference/Selector selector/parent) combinands))]
    [(Selector-Prefixed prefix selector subject)
     (Selector-Prefixed
      prefix
      (and selector (replace-parent-reference/Selector selector/parent selector))
      (match subject
        [(? symbol?) subject]
        [(? Selector-FunctionApplication?)
         (replace-parent-reference/Selector-FunctionApplication selector/parent subject)]))]
    [(Selector-Attribute selector subject)
     (Selector-Attribute
      (and selector (replace-parent-reference/Selector selector/parent selector)) subject)]
    [(? Selector?) selector/child]))

(: replace-parent-reference/Selector-FunctionApplication
   (-> Selector Selector-FunctionApplication Selector-FunctionApplication))
(define (replace-parent-reference/Selector-FunctionApplication
         selector/parent selector/function-application)
  (match-define (Selector-FunctionApplication function arguments) selector/function-application)
  (Selector-FunctionApplication
   function
   (for/list : (Listof (U Selector Selector-FunctionApplication Selector-An+B)) ([argument arguments])
     (match argument
       [(? Selector?) (replace-parent-reference/Selector selector/parent argument)]
       [(? Selector-FunctionApplication?)
        (replace-parent-reference/Selector-FunctionApplication selector/parent argument)]
       [(? Selector-An+B?) argument]))))

;; ---------------------------------------------------------------------------------------------------

(: merge/AtRule-Expressions
   (-> (Listof AtRule-Expression) (Listof AtRule-Expression) (Listof AtRule-Expression)))
(define (merge/AtRule-Expressions at-rule/expressions/parent at-rule/expressions/child)
  (for/list ([at-rule/expressions/pair
              (cartesian-product at-rule/expressions/parent at-rule/expressions/child)])
    (AtRule-Expression-Operation-NAry 'and at-rule/expressions/pair)))