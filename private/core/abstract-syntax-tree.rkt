#lang typed/racket/base
(provide (all-defined-out))

(struct Stylesheet
  ([rules : (Listof Rule)])
  #:transparent)

(struct Rule () #:transparent)

(struct QualifiedRule Rule
  ([selectors : (Listof Selector)]
   [declarations : (Listof Declaration)])
  #:transparent)

(struct AtRule Rule
  ([name : Symbol]
   [expressions : (Listof AtRule-Expression)]
   [elements : (Option (Listof (U QualifiedRule Declaration)))])
  #:transparent)

(struct AtRule-Expression () #:transparent)

(struct AtRule-Expression-Declaration AtRule-Expression
  ([declaration : Declaration])
  #:transparent)

(struct AtRule-Expression-Value AtRule-Expression
  ([value : Value])
  #:transparent)

(struct AtRule-Expression-Operation AtRule-Expression () #:transparent)

(struct AtRule-Expression-Operation-Unary AtRule-Expression-Operation
  ([operator : (U 'not 'only)]
   [operand : AtRule-Expression])
  #:transparent)

(struct AtRule-Expression-Operation-NAry AtRule-Expression-Operation
  ([operator : (U '| | 'and 'or)]
   [operands : (Listof AtRule-Expression)])
  #:transparent)

(struct Declaration
  ([name : Symbol]
   [values : (Listof Value)]
   [important? : Boolean])
  #:transparent)

(struct Value () #:transparent)

(struct Value-Literal Value
  ([literal : (U Symbol Number String)])
  #:transparent)

(struct Value-Measurement Value
  ([unit : (U '% 'em 'ex 'ch 'rem 'vw 'vh 'vmin 'vmax 'cm 'mm 'q
              'in 'pt 'pc 'px 'deg 'grad 'rad 'turn 's 'ms 'hz 'khz
              'dpi 'dpcm 'dppx)]
   [magnitude : Number])
  #:transparent)

(struct Value-FunctionApplication Value
  ([function : Symbol]
   [arguments : (Listof Value)])
  #:transparent)

(struct Value-Operation Value
  ([operator : (U '| | '+ '- '* '/)]
   [operands : (Listof Value)])
  #:transparent)

(struct Selector () #:transparent)

(struct Selector-Tag Selector
  ([tag : Symbol])
  #:transparent)

(struct Selector-Namespaced Selector
  ([namespace : (Option Symbol)]
   [name : Symbol])
  #:transparent)

(struct Selector-Combination Selector
  ([combinator : (U '| | '+ '> '~ Selector-Combination-Combinator-Reference '\|\|)]
   [combinands : (Listof Selector)])
  #:transparent)

(struct Selector-Combination-Combinator-Reference
  ([reference : Symbol])
  #:transparent)

(struct Selector-Prefixed Selector
  ([prefix : (U '\. '\# ': '::)]
   [selector : (Option Selector)]
   [subject : (U Symbol Selector-FunctionApplication)])
  #:transparent)

(struct Selector-FunctionApplication
  ([function : Symbol]
   [arguments : (Listof (U Selector Selector-FunctionApplication Selector-An+B))])
  #:transparent)

(struct Selector-Attribute Selector
  ([selector : (Option Selector)]
   [subject : (U Symbol Selector-Namespaced Selector-Attribute-Operation)])
  #:transparent)

(struct Selector-Attribute-Operation
  ([operator : (U '= '~= '^= '$= '*= '\|=)]
   [operands : (Pairof Selector-Attribute-Operation-Operand Selector-Attribute-Operation-Operand)])
  #:transparent)

(struct Selector-Attribute-Operation-Operand () #:transparent)

(struct Selector-Attribute-Operation-Operand-Literal Selector-Attribute-Operation-Operand
  ([literal : (U Symbol String)])
  #:transparent)

(struct Selector-Attribute-Operation-Operand-CaseInsensitive Selector-Attribute-Operation-Operand
  ([string : String])
  #:transparent)

(struct Selector-An+B () #:transparent)

(struct Selector-An+B-Literal Selector-An+B
  ([literal : (U 'even 'odd Integer)])
  #:transparent)

(struct Selector-An+B-Proper Selector-An+B
  ([step : (Option Integer)]
   [offset : (Option Integer)])
  #:transparent)