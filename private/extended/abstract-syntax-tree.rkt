#lang typed/racket/base
(require (prefix-in Core: "../core/abstract-syntax-tree.rkt"))

(provide (all-defined-out))

(struct QualifiedRule Core:Rule
  ([selectors : (Listof Core:Selector)]
   [elements : (Listof (U Core:Rule Declaration))])
  #:transparent)

(struct AtRule Core:Rule
  ([name : Symbol]
   [expressions : (Listof Core:AtRule-Expression)]
   [elements : (Option (Listof (U Core:Rule Declaration)))])
  #:transparent)

(struct Declaration Core:Declaration
  ([nested-declarations : (Option (Listof Declaration))])
  #:transparent)

(struct Selector-Parent Core:Selector
  ([suffix : (Option Symbol)])
  #:transparent)