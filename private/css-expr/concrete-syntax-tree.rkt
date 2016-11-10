#lang racket/base
(require syntax/parse)

(provide (all-defined-out))

(define-syntax-class stylesheet
  (pattern (rule:rule ...)))

(define-syntax-class rule
  (pattern [(~datum @) name:identifier expression:at-rule/expression ...
                       block-element:block-element ...])
  (pattern [selector:selector ...+ block-element:block-element ...+]))

(define-splicing-syntax-class block-element
  (pattern element:declaration)
  (pattern element:rule))

(define-syntax-class at-rule/expression
  (pattern (expression/declaration:declaration))
  (pattern (operator:at-rule/expression/operation/operator/unary operand:at-rule/expression))
  (pattern (operator:at-rule/expression/operation/operator/n-ary operand:at-rule/expression ...+))
  (pattern expression/value:value)
  (pattern (expression:at-rule/expression ...+)))

(define-syntax-class at-rule/expression/operation/operator/unary
  (pattern (~datum not))
  (pattern (~datum only)))

(define-syntax-class at-rule/expression/operation/operator/n-ary
  (pattern (~datum and))
  (pattern (~datum or)))

(define-splicing-syntax-class declaration
  (pattern (~seq name:keyword value:value ...+
                 (~optional (~and (~datum !important) important?))
                 (~optional (nested-declaration:declaration ...+))))
  (pattern (~seq name:keyword (nested-declaration:declaration ...+))))

(define-syntax-class value
  (pattern value/identifier:value/identifier)
  (pattern value/number:number)
  (pattern value/string:str)
  (pattern (unit:value/measurement/unit magnitude:number))
  (pattern ((~datum apply) name:identifier argument:value ...+))
  (pattern (operator:value/operation/operator operand:value ...+))
  (pattern (value/list:value ...+)))

(define-syntax-class value/identifier
  (pattern (~and value/identifier:identifier
                 (~not (~datum !important))
                 (~not (~datum @)))))

(define-syntax-class value/measurement/unit
  (pattern (~datum %))
  (pattern (~datum em))
  (pattern (~datum ex))
  (pattern (~datum ch))
  (pattern (~datum rem))
  (pattern (~datum vw))
  (pattern (~datum vh))
  (pattern (~datum vmin))
  (pattern (~datum vmax))
  (pattern (~datum cm))
  (pattern (~datum mm))
  (pattern (~datum q))
  (pattern (~datum in))
  (pattern (~datum pt))
  (pattern (~datum pc))
  (pattern (~datum px))
  (pattern (~datum deg))
  (pattern (~datum grad))
  (pattern (~datum rad))
  (pattern (~datum turn))
  (pattern (~datum s))
  (pattern (~datum ms))
  (pattern (~datum hz))
  (pattern (~datum khz))
  (pattern (~datum dpi))
  (pattern (~datum dpcm))
  (pattern (~datum dppx)))

(define-syntax-class value/operation/operator
  (pattern (~datum +))
  (pattern (~datum -))
  (pattern (~datum *))
  (pattern (~datum /)))

(define-syntax-class selector
  (pattern (~datum &))
  (pattern ((~datum &-) suffix:identifier))
  (pattern selector/namespaced:selector/namespaced)
  (pattern (prefix:selector/prefixed/prefix (~optional selector:selector)
                                            subject:selector/prefixed/subject))
  (pattern ((~datum attribute) (~optional selector:selector) subject:selector/attribute/subject))
  (pattern (combinator:selector/combination/combinator combinand:selector ...+))
  (pattern (selector/list:selector ...+))
  (pattern selector/identifier:identifier))

(define-syntax-class selector/namespaced
  (pattern ((~datum \|) (~optional namespace:identifier) element:identifier)))

(define-syntax-class selector/prefixed/prefix
  (pattern (~datum \.))
  (pattern (~datum \#))
  (pattern (~datum :))
  (pattern (~datum ::)))

(define-syntax-class selector/prefixed/subject
  (pattern subject:identifier)
  (pattern subject:selector/function/application))

(define-syntax-class selector/function/application
  (pattern ((~datum apply) name:identifier argument:selector/function/argument ...+)))

(define-syntax-class selector/function/argument
  (pattern argument:selector/An+B)
  (pattern argument:selector)
  (pattern argument:selector/function/application))

(define-syntax-class selector/An+B
  (pattern (~datum odd))
  (pattern (~datum even))
  (pattern An+B:integer)
  (pattern (~datum n))
  (pattern ((~datum n) step:integer))
  (pattern ((~datum n) step:integer offset:integer))
  (pattern ((~datum n+) offset:integer)))

(define-syntax-class selector/attribute/subject
  (pattern subject:identifier)
  (pattern subject:selector/namespaced)
  (pattern (operator:selector/attribute/operation/operator
            operand/left:selector/attribute/operation/operand
            operand/right:selector/attribute/operation/operand)))

(define-syntax-class selector/attribute/operation/operator
  (pattern (~datum =))
  (pattern (~datum ~=))
  (pattern (~datum ^=))
  (pattern (~datum $=))
  (pattern (~datum *=))
  (pattern (~datum \|=)))

(define-syntax-class selector/attribute/operation/operand
  (pattern operand:identifier)
  (pattern operand:str)
  (pattern ((~datum case-insensitive) string:str)))

(define-syntax-class selector/combination/combinator
  (pattern (~datum +))
  (pattern (~datum >))
  (pattern (~datum ~))
  (pattern ((~datum //) reference:identifier))
  (pattern (~datum \|\|)))