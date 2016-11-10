#lang typed/racket/base

(module parser racket/base
  (require "concrete-syntax-tree.rkt"
           "../core/abstract-syntax-tree.rkt"
           (prefix-in Extended: "../extended/abstract-syntax-tree.rkt")
           syntax/parse racket/list)

  (provide parse)

  (define parse/stylesheet
    (syntax-parser
      [stylesheet:stylesheet
       (Stylesheet (map parse/rule (syntax->list #'(stylesheet.rule ...))))]))

  (define parse/rule
    (syntax-parser
      [[(~datum @) name:identifier expression:at-rule/expression ...
                   block-element:block-element ...+]
       (Extended:AtRule (syntax->datum #'name)
                        (map parse/at-rule/expression (syntax->list #'(expression ...)))
                        (map parse/block-element (syntax->list #'(block-element ...))))]
      [[(~datum @) name:identifier expression:at-rule/expression ...]
       (Extended:AtRule (syntax->datum #'name)
                        (map parse/at-rule/expression (syntax->list #'(expression ...)))
                        #f)]
      [[selector:selector ...+ block-element:block-element ...+]
       (Extended:QualifiedRule (map parse/selector (syntax->list #'(selector ...)))
                               (map parse/block-element (syntax->list #'(block-element ...))))]))

  (define parse/block-element
    (syntax-parser
      [(element:declaration) (parse/declaration #'element)]
      [(element:rule) (parse/rule #'element)]))

  (define parse/at-rule/expression
    (syntax-parser
      [(expression/declaration:declaration)
       (AtRule-Expression-Declaration (parse/declaration #'expression/declaration))]
      [(operator:at-rule/expression/operation/operator/unary operand:at-rule/expression)
       (AtRule-Expression-Operation-Unary
        (parse/at-rule/expression/operation/operator/unary #'operator)
        (parse/at-rule/expression #'operand))]
      [(operator:at-rule/expression/operation/operator/n-ary operand:at-rule/expression ...+)
       (AtRule-Expression-Operation-NAry
        (parse/at-rule/expression/operation/operator/n-ary #'operator)
        (map parse/at-rule/expression (syntax->list #'(operand ...))))]
      [expression/value:value (AtRule-Expression-Value (parse/value #'expression/value))]
      [(expression:at-rule/expression ...+)
       (AtRule-Expression-Operation-NAry
        '| | (map parse/at-rule/expression (syntax->list #'(expression ...))))]))

  (define parse/at-rule/expression/operation/operator/unary
    (syntax-parser
      [operator:at-rule/expression/operation/operator/unary (syntax->datum #'operator)]))

  (define parse/at-rule/expression/operation/operator/n-ary
    (syntax-parser
      [operator:at-rule/expression/operation/operator/n-ary (syntax->datum #'operator)]))

  (define parse/declaration
    (syntax-parser
      [(name:keyword value:value ...+
                     (~optional (~and (~datum !important) important?))
                     (~optional (nested-declaration:declaration ...+)))
       (Extended:Declaration
        (string->symbol (keyword->string (syntax->datum #'name)))
        (map parse/value (syntax->list #'(value ...)))
        (if (attribute important?) #t #f)
        (and (attribute nested-declaration)
             (map parse/declaration (syntax->list #'(nested-declaration ...)))))]
      [(name:keyword (~optional (nested-declaration:declaration ...+)))
       (Extended:Declaration
        (string->symbol (keyword->string (syntax->datum #'name)))
        empty
        #f
        (and (attribute nested-declaration)
             (map parse/declaration (syntax->list #'(nested-declaration ...)))))]))

  (define parse/value
    (syntax-parser
      [value/identifier:value/identifier
       (Value-Literal (syntax->datum #'value/identifier))]
      [value/number:number (Value-Literal (syntax->datum #'value/number))]
      [value/string:str (Value-Literal (syntax->datum #'value/string))]
      [(unit:value/measurement/unit magnitude:number)
       (Value-Measurement (parse/value/measurement/unit #'unit)
                          (syntax->datum #'magnitude))]
      [((~datum apply) name:identifier argument:value ...+)
       (Value-FunctionApplication (syntax->datum #'name)
                                  (map parse/value (syntax->list #'(argument ...))))]
      [(operator:value/operation/operator operand:value ...+)
       (Value-Operation (parse/value/operation/operator #'operator)
                        (map parse/value (syntax->list #'(operand ...))))]
      [(value/list:value ...+)
       (Value-Operation '| | (map parse/value (syntax->list #'(value/list ...))))]))

  (define parse/value/measurement/unit
    (syntax-parser
      [unit:value/measurement/unit (syntax->datum #'unit)]))

  (define parse/value/operation/operator
    (syntax-parser
      [operator:value/operation/operator (syntax->datum #'operator)]))

  (define parse/selector
    (syntax-parser
      [(~datum &) (Extended:Selector-Parent #f)]
      [((~datum &-) suffix:identifier)
       (Extended:Selector-Parent (syntax->datum #'suffix))]
      [selector/namespaced:selector/namespaced
       (parse/selector/namespaced #'selector/namespaced)]
      [(prefix:selector/prefixed/prefix (~optional selector:selector)
                                        subject:selector/prefixed/subject)
       (Selector-Prefixed (parse/selector/prefixed/prefix #'prefix)
                          (and (attribute selector) (parse/selector #'selector))
                          (parse/selector/prefixed/subject #'subject))]
      [((~datum attribute) (~optional selector:selector) subject:selector/attribute/subject)
       (Selector-Attribute (and (attribute selector) (parse/selector #'selector))
                           (parse/selector/attribute/subject #'subject))]
      [(combinator:selector/combination/combinator combinand:selector ...+)
       (Selector-Combination (parse/selector/combination/combinator #'combinator)
                             (map parse/selector (syntax->list #'(combinand ...))))]
      [(selector/list:selector ...+)
       (Selector-Combination '| | (map parse/selector (syntax->list #'(selector/list ...))))]
      [selector/identifier:identifier (Selector-Tag (syntax->datum #'selector/identifier))]))

  (define parse/selector/namespaced
    (syntax-parser
      [selector:selector/namespaced
       (Selector-Namespaced (and (attribute selector.namespace)
                                 (syntax->datum #'selector.namespace))
                            (syntax->datum #'selector.element))]))

  (define parse/selector/prefixed/prefix
    (syntax-parser
      [prefix:selector/prefixed/prefix (syntax->datum #'prefix)]))

  (define parse/selector/prefixed/subject
    (syntax-parser
      [subject:identifier (syntax->datum #'subject)]
      [subject:selector/function/application (parse/selector/function/application #'subject)]))

  (define parse/selector/function/application
    (syntax-parser
      [selector:selector/function/application
       (Selector-FunctionApplication
        (syntax->datum #'selector.name)
        (map parse/selector/function/argument (syntax->list #'(selector.argument ...))))]))

  (define parse/selector/function/argument
    (syntax-parser
      [argument:selector/An+B (parse/selector/An+B #'argument)]
      [argument:selector/function/application (parse/selector/function/application #'argument)]
      [argument:selector (parse/selector #'argument)]))

  (define parse/selector/An+B
    (syntax-parser
      [(~datum odd) (Selector-An+B-Literal 'odd)]
      [(~datum even) (Selector-An+B-Literal 'even)]
      [An+B:integer (Selector-An+B-Literal (syntax->datum #'An+B))]
      [(~datum n) (Selector-An+B-Proper #f #f)]
      [((~datum n) step:integer) (Selector-An+B-Proper (syntax->datum #'step) #f)]
      [((~datum n) step:integer offset:integer)
       (Selector-An+B-Proper (syntax->datum #'step) (syntax->datum #'offset))]
      [((~datum n+) offset:integer) (Selector-An+B-Proper #f (syntax->datum #'offset))]))

  (define parse/selector/attribute/subject
    (syntax-parser
      [subject:identifier (syntax->datum #'subject)]
      [subject:selector/namespaced (parse/selector/namespaced #'subject)]
      [(operator:selector/attribute/operation/operator
        operand/left:selector/attribute/operation/operand
        operand/right:selector/attribute/operation/operand)
       (Selector-Attribute-Operation
        (parse/selector/attribute/operation/operator #'operator)
        (cons (parse/selector/attribute/operation/operand #'operand/left)
              (parse/selector/attribute/operation/operand #'operand/right)))]))

  (define parse/selector/attribute/operation/operator
    (syntax-parser
      [operator:selector/attribute/operation/operator (syntax->datum #'operator)]))

  (define parse/selector/attribute/operation/operand
    (syntax-parser
      [operand:identifier (Selector-Attribute-Operation-Operand-Literal (syntax->datum #'operand))]
      [operand:str (Selector-Attribute-Operation-Operand-Literal (syntax->datum #'operand))]
      [((~datum case-insensitive) string:str)
       (Selector-Attribute-Operation-Operand-CaseInsensitive (syntax->datum #'string))]))

  (define parse/selector/combination/combinator
    (syntax-parser
      [((~datum //) reference:identifier)
       (Selector-Combination-Combinator-Reference (syntax->datum #'reference))]
      [combinator:selector/combination/combinator (syntax->datum #'combinator)]))

  (define parse parse/stylesheet))

(require "../core/abstract-syntax-tree.rkt")

(require/typed/provide 'parser [parse (-> Sexp Stylesheet)])