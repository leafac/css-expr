#lang racket/base
(require syntax/parse syntax/parse/define
         nanopass/base
         racket/string racket/list racket/format racket/dict racket/function racket/match)

(provide css-expr css-expr->css)

(module+ test
  (require rackunit))

(define-language css/core
  (terminals
   (symbol (Symbol))
   (string (String))
   (integer (Integer))
   (number (Number))
   (boolean (Boolean))
   (Symbols/AtRule-Expression-Operation-Unary-Operator
    (Symbols/AtRule-Expression-Operation-Unary-Operator))
   (Symbols/AtRule-Expression-Operation-NAry-Operator
    (Symbols/AtRule-Expression-Operation-NAry-Operator))
   (Symbols/Value-Measurement-Unit (Symbols/Value-Measurement-Unit))
   (Symbols/Value-Operation-Operator (Symbols/Value-Operation-Operator))
   (Symbols/Selector-Combination-Combinator-Literal (Symbols/Selector-Combination-Combinator-Literal))
   (Symbols/Selector-Prefixed-Prefix (Symbols/Selector-Prefixed-Prefix))
   (Symbols/Selector-FunctionApplication-Argument-An+B-Literal
    (Symbols/Selector-FunctionApplication-Argument-An+B-Literal))
   (Symbols/Selector-Attribute-Operation-Operator (Symbols/Selector-Attribute-Operation-Operator)))

  (Stylesheet (Stylesheet) (stylesheet (Rule ...)))
  (Rule (Rule) QualifiedRule AtRule)
  (QualifiedRule (QualifiedRule) (qualified-rule (Selector ...) (QualifiedRule-Element ...)))
  (QualifiedRule-Element (QualifiedRule-Element) Declaration)
  (AtRule (AtRule) (at-rule AtRule-Name (AtRule-Expression ...) (AtRule-Element ...)))
  (AtRule-Name (AtRule-Name) Symbol)
  (AtRule-Expression (AtRule-Expression) Declaration Value AtRule-Expression-Operation)
  (AtRule-Expression-Operation
   (AtRule-Expression-Operation) AtRule-Expression-Operation-Unary AtRule-Expression-Operation-NAry)
  (AtRule-Expression-Operation-Unary
   (AtRule-Expression-Operation-Unary)
   (at-rule/expression/operation/unary AtRule-Expression-Operation-Unary-Operator AtRule-Expression))
  (AtRule-Expression-Operation-Unary-Operator
   (AtRule-Expression-Operation-Unary-Operator) Symbols/AtRule-Expression-Operation-Unary-Operator)
  (AtRule-Expression-Operation-NAry
   (AtRule-Expression-Operation-NAry)
   (at-rule/expression/operation/n-ary AtRule-Expression-Operation-NAry-Operator
                                       (AtRule-Expression ...)))
  (AtRule-Expression-Operation-NAry-Operator
   (AtRule-Expression-Operation-NAry-Operator) Symbols/AtRule-Expression-Operation-NAry-Operator)
  (AtRule-Element (AtRule-Element) QualifiedRule Declaration)
  (Declaration (Declaration) (declaration Declaration-Name (Value ...) Declaration-Important))
  (Declaration-Name (Declaration-Name) Symbol)
  (Declaration-Important (Declaration-Important) Boolean)
  (Value (Value) Value-Literal Value-Measurement Value-FunctionApplication Value-Operation)
  (Value-Literal (Value-Literal) Symbol Number String)
  (Value-Measurement
   (Value-Measurement) (value/measurement Value-Measurement-Unit Value-Measurement-Magnitude))
  (Value-Measurement-Unit (Value-Measurement-Unit) Symbols/Value-Measurement-Unit)
  (Value-Measurement-Magnitude (Value-Measurement-Magnitude) Number)
  (Value-FunctionApplication
   (Value-FunctionApplication)
   (value/function-application Value-FunctionApplication-Function (Value ...)))
  (Value-FunctionApplication-Function (Value-FunctionApplication-Function) Symbol)
  (Value-Operation (Value-Operation) (value-operation Value-Operation-Operator (Value ...)))
  (Value-Operation-Operator (Value-Operation-Operator) Symbols/Value-Operation-Operator)
  (Selector
   (Selector) Selector-Tag Selector-Namespaced Selector-Combination Selector-Prefixed
   Selector-Attribute)
  (Selector-Tag (Selector-Tag) Symbol)
  (Selector-Namespaced
   (Selector-Namespaced)
   (selector/namespaced (maybe Selector-Namespaced-Namespace) Selector-Namespaced-Name))
  (Selector-Namespaced-Namespace (Selector-Namespaced-Namespace) Symbol)
  (Selector-Namespaced-Name (Selector-Namespaced-Name) Symbol)
  (Selector-Combination
   (Selector-Combination) (selector/combination Selector-Combination-Combinator (Selector ...)))
  (Selector-Combination-Combinator
   (Selector-Combination-Combinator)
   Selector-Combination-Combinator-Literal Selector-Combination-Combinator-Reference)
  (Selector-Combination-Combinator-Literal
   (Selector-Combination-Combinator-Literal) Symbols/Selector-Combination-Combinator-Literal)
  (Selector-Combination-Combinator-Reference
   (Selector-Combination-Combinator-Reference) (selector/combination/combinator/reference Symbol))
  (Selector-Prefixed
   (Selector-Prefixed)
   (selector/prefixed Selector-Prefixed-Prefix (maybe Selector) Selector-Prefixed-Subject))
  (Selector-Prefixed-Prefix (Selector-Prefixed-Prefix) Symbols/Selector-Prefixed-Prefix)
  (Selector-Prefixed-Subject
   (Selector-Prefixed-Subject) Selector-Prefixed-Subject-Name Selector-FunctionApplication)
  (Selector-Prefixed-Subject-Name (Selector-Prefixed-Subject-Name) Symbol)
  (Selector-FunctionApplication
   (Selector-FunctionApplication)
   (selector/function-application Selector-FunctionApplication-Function
                                  (Selector-FunctionApplication-Argument ...)))
  (Selector-FunctionApplication-Function (Selector-FunctionApplication-Function) Symbol)
  (Selector-FunctionApplication-Argument
   (Selector-FunctionApplication-Argument)
   Selector Selector-FunctionApplication Selector-FunctionApplication-Argument-An+B)
  (Selector-FunctionApplication-Argument-An+B
   (Selector-FunctionApplication-Argument-An+B)
   Selector-FunctionApplication-Argument-An+B-Literal
   Selector-FunctionApplication-Argument-An+B-Proper)
  (Selector-FunctionApplication-Argument-An+B-Literal
   (Selector-FunctionApplication-Argument-An+B-Literal)
   Symbols/Selector-FunctionApplication-Argument-An+B-Literal Integer)
  (Selector-FunctionApplication-Argument-An+B-Proper
   (Selector-FunctionApplication-Argument-An+B-Proper)
   (selector/function-application/argument/An+B/proper
    (maybe Selector-FunctionApplication-Argument-An+B-Proper-Step)
    (maybe Selector-FunctionApplication-Argument-An+B-Proper-Offset)))
  (Selector-FunctionApplication-Argument-An+B-Proper-Step
   (Selector-FunctionApplication-Argument-An+B-Proper-Step) Integer)
  (Selector-FunctionApplication-Argument-An+B-Proper-Offset
   (Selector-FunctionApplication-Argument-An+B-Proper-Offset) Integer)
  (Selector-Attribute
   (Selector-Attribute) (selector/attribute (maybe Selector) Selector-Attribute-Subject))
  (Selector-Attribute-Subject
   (Selector-Attribute-Subject)
   Selector-Attribute-Subject-Literal Selector-Namespaced Selector-Attribute-Operation)
  (Selector-Attribute-Subject-Literal (Selector-Attribute-Subject-Literal) Symbol)
  (Selector-Attribute-Operation
   (Selector-Attribute-Operation)
   (selector/attribute/operation Selector-Attribute-Operation-Operator
                                 Selector-Attribute-Operation-Operand1
                                 Selector-Attribute-Operation-Operand2))
  (Selector-Attribute-Operation-Operator
   (Selector-Attribute-Operation-Operator) Symbols/Selector-Attribute-Operation-Operator)
  (Selector-Attribute-Operation-Operand
   (Selector-Attribute-Operation-Operand)
   Selector-Attribute-Operation-Operand-Literal Selector-Attribute-Operation-Operand-CaseInsensitive)
  (Selector-Attribute-Operation-Operand-Literal
   (Selector-Attribute-Operation-Operand-Literal) Symbol String)
  (Selector-Attribute-Operation-Operand-CaseInsensitive
   (Selector-Attribute-Operation-Operand-CaseInsensitive)
   (selector/attribute/operation/operand/case-insensitive String)))

(define (Symbols/AtRule-Expression-Operation-Unary-Operator? symbol)
  (member symbol '(not only)))

(define (Symbols/AtRule-Expression-Operation-NAry-Operator? symbol)
  (member symbol '(| | and or)))

(define (Symbols/Value-Measurement-Unit? symbol)
  (member symbol '(% em ex ch rem vw vh vmin vmax cm mm q in pt pc px deg grad rad turn s ms hz khz
                     dpi dpcm dppx)))

(define (Symbols/Value-Operation-Operator? symbol)
  (member symbol '(| | + - * /)))

(define (Symbols/Selector-Combination-Combinator-Literal? symbol)
  (member symbol '(| | + > ~ \|\|)))

(define (Symbols/Selector-Prefixed-Prefix? symbol)
  (member symbol '(\. \# : ::)))

(define (Symbols/Selector-FunctionApplication-Argument-An+B-Literal? symbol)
  (member symbol '(even odd)))

(define (Symbols/Selector-Attribute-Operation-Operator? symbol)
  (member symbol '(= ~= ^= $= *= \|=)))

;; ---------------------------------------------------------------------------------------------------

(define-pass css/core->string : css/core (Stylesheet*) -> * ()
  (definitions
    (define current-string (make-parameter ""))
    (define (emit . string)
      (current-string (string-join `(,(current-string) ,@string) "")))
    (define-simple-macro (collect expression:expr ...+)
      (parameterize ([current-string ""])
        expression ...
        (current-string))))

  (Stylesheet : Stylesheet (Stylesheet*) -> * ()
              [(stylesheet (,Rule* ...)) (for-each Rule Rule*)])
  (Rule : Rule (Rule*) -> * ()
        [,QualifiedRule* (QualifiedRule QualifiedRule*)]
        [,AtRule* (AtRule AtRule*)])
  (QualifiedRule : QualifiedRule (QualifiedRule*) -> * ()
                 [(qualified-rule (,Selector* ...) (,QualifiedRule-Element* ...))
                  (emit
                   (string-join
                    (for/list ([selector (in-list Selector*)])
                      (collect (Selector selector)))
                    ","))
                  (emit "{")
                  (for-each QualifiedRule-Element QualifiedRule-Element*)
                  (emit "}")])
  (QualifiedRule-Element : QualifiedRule-Element (QualifiedRule-Element*) -> * ()
                         [,Declaration* (Declaration Declaration*) (emit ";")])
  (AtRule : AtRule (AtRule*) -> * ()
          [(at-rule ,AtRule-Name* (,AtRule-Expression* ...) (,AtRule-Element* ...))
           (emit "@")
           (AtRule-Name AtRule-Name*)
           (unless (empty? AtRule-Expression*) (emit " "))
           (emit
            (string-join
             (for/list ([at-rule/expression (in-list AtRule-Expression*)])
               (collect (AtRule-Expression at-rule/expression)))
             ","))
           (cond
             [(empty? AtRule-Element*) (emit ";")]
             [else
              (emit "{")
              (for-each AtRule-Element AtRule-Element*)
              (emit "}")])])
  (AtRule-Name : AtRule-Name (AtRule-Name*) -> * () [,Symbol* (emit (~a Symbol*))])
  (AtRule-Expression : AtRule-Expression (AtRule-Expression*) -> * ()
                     [,Declaration* (emit "(") (Declaration Declaration*) (emit ")")]
                     [,Value* (Value Value*)]
                     [,AtRule-Expression-Operation*
                      (AtRule-Expression-Operation AtRule-Expression-Operation*)])
  (AtRule-Expression-Operation
   : AtRule-Expression-Operation (AtRule-Expression-Operation*) -> * ()
   [,AtRule-Expression-Operation-Unary*
    (AtRule-Expression-Operation-Unary AtRule-Expression-Operation-Unary*)]
   [,AtRule-Expression-Operation-NAry*
    (AtRule-Expression-Operation-NAry AtRule-Expression-Operation-NAry*)])
  (AtRule-Expression-Operation-Unary
   : AtRule-Expression-Operation-Unary (AtRule-Expression-Operation-Unary*) -> * ()
   [(at-rule/expression/operation/unary
     ,AtRule-Expression-Operation-Unary-Operator*
     ,AtRule-Expression*)
    (AtRule-Expression-Operation-Unary-Operator AtRule-Expression-Operation-Unary-Operator*)
    (AtRule-Expression AtRule-Expression*)])
  (AtRule-Expression-Operation-Unary-Operator
   : AtRule-Expression-Operation-Unary-Operator (AtRule-Expression-Operation-Unary-Operator*) -> * ()
   [,Symbols/AtRule-Expression-Operation-Unary-Operator*
    (emit (~a Symbols/AtRule-Expression-Operation-Unary-Operator*))
    (emit " ")])
  (AtRule-Expression-Operation-NAry
   : AtRule-Expression-Operation-NAry (AtRule-Expression-Operation-NAry*) -> * ()
   [(at-rule/expression/operation/n-ary ,AtRule-Expression-Operation-NAry-Operator*
                                        (,AtRule-Expression* ...))
    (emit
     (string-join
      (for/list ([at-rule/expression (in-list AtRule-Expression*)])
        (collect (AtRule-Expression at-rule/expression)))
      (collect
       (AtRule-Expression-Operation-NAry-Operator AtRule-Expression-Operation-NAry-Operator*))))])
  (AtRule-Expression-Operation-NAry-Operator
   : AtRule-Expression-Operation-NAry-Operator (AtRule-Expression-Operation-NAry-Operator*) -> * ()
   [,Symbols/AtRule-Expression-Operation-NAry-Operator*
    (case Symbols/AtRule-Expression-Operation-NAry-Operator*
      [(| |) (emit (~a Symbols/AtRule-Expression-Operation-NAry-Operator*))]
      [else
       (emit " ")
       (emit (~a Symbols/AtRule-Expression-Operation-NAry-Operator*))
       (emit " ")])])
  (AtRule-Element : AtRule-Element (AtRule-Element*) -> * ()
                  [,QualifiedRule* (QualifiedRule QualifiedRule*)]
                  [,Declaration* (Declaration Declaration*) (emit ";")])
  (Declaration : Declaration (Declaration*) -> * ()
               [(declaration ,Declaration-Name* (,Value* ...) ,Declaration-Important*)
                (Declaration-Name Declaration-Name*)
                (emit ":")
                (emit
                 (string-join
                  (for/list ([value (in-list Value*)])
                    (collect (Value value)))
                  ","))
                (Declaration-Important Declaration-Important*)])
  (Declaration-Name : Declaration-Name (Declaration-Name*) -> * ()
                    [,Symbol* (emit (~a Symbol*))])
  (Declaration-Important : Declaration-Important (Declaration-Important*) -> * ()
                         [,Boolean* (when Boolean* (emit " !important"))])
  (Value : Value (Value*) -> * ()
         [,Value-Literal* (Value-Literal Value-Literal*)]
         [,Value-Measurement* (Value-Measurement Value-Measurement*)]
         [,Value-FunctionApplication* (Value-FunctionApplication Value-FunctionApplication*)]
         [,Value-Operation* (Value-Operation Value-Operation*)])
  (Value-Literal : Value-Literal (Value-Literal*) -> * ()
                 [,Symbol* (emit (~a Symbol*))]
                 [,Number* (emit (~a Number*))]
                 [,String* (emit "\"") (emit String*) (emit "\"")])
  (Value-Measurement : Value-Measurement (Value-Measurement*) -> * ()
                     [(value/measurement ,Value-Measurement-Unit* ,Value-Measurement-Magnitude*)
                      (Value-Measurement-Magnitude Value-Measurement-Magnitude*)
                      (Value-Measurement-Unit Value-Measurement-Unit*)])
  (Value-Measurement-Unit : Value-Measurement-Unit (Value-Measurement-Unit*) -> * ()
                          [,Symbols/Value-Measurement-Unit*
                           (emit (~a Symbols/Value-Measurement-Unit*))])
  (Value-Measurement-Magnitude : Value-Measurement-Magnitude (Value-Measurement-Magnitude*) -> * ()
                               [,Number* (emit (~a Number*))])
  (Value-FunctionApplication
   : Value-FunctionApplication (Value-FunctionApplication*) -> * ()
   [(value/function-application ,Value-FunctionApplication-Function* (,Value* ...))
    (Value-FunctionApplication-Function Value-FunctionApplication-Function*)
    (emit "(")
    (emit
     (string-join
      (for/list ([value (in-list Value*)])
        (collect (Value value)))
      ","))
    (emit ")")])
  (Value-FunctionApplication-Function
   : Value-FunctionApplication-Function (Value-FunctionApplication-Function*) -> * ()
   [,Symbol* (emit (~a Symbol*))])
  (Value-Operation : Value-Operation (Value-Operation*) -> * ()
                   [(value-operation ,Value-Operation-Operator* (,Value* ...))
                    (emit
                     (string-join
                      (for/list ([value (in-list Value*)])
                        (collect (Value value)))
                      (collect (Value-Operation-Operator Value-Operation-Operator*))))])
  (Value-Operation-Operator : Value-Operation-Operator (Value-Operation-Operator*) -> * ()
                            [,Symbols/Value-Operation-Operator*
                             (case Symbols/Value-Operation-Operator*
                               [(| | / *) (emit (~a Symbols/Value-Operation-Operator*))]
                               [else
                                (emit " ")
                                (emit (~a Symbols/Value-Operation-Operator*))
                                (emit " ")])])
  (Selector : Selector (Selector*) -> * ()
            [,Selector-Tag* (Selector-Tag Selector-Tag*)]
            [,Selector-Namespaced* (Selector-Namespaced Selector-Namespaced*)]
            [,Selector-Combination* (Selector-Combination Selector-Combination*)]
            [,Selector-Prefixed* (Selector-Prefixed Selector-Prefixed*)]
            [,Selector-Attribute* (Selector-Attribute Selector-Attribute*)])
  (Selector-Tag : Selector-Tag (Selector-Tag*) -> * ()
                [,Symbol* (emit (~a Symbol*))])
  (Selector-Namespaced : Selector-Namespaced (Selector-Namespaced*) -> * ()
                       [(selector/namespaced ,Selector-Namespaced-Namespace*
                                             ,Selector-Namespaced-Name*)
                        (when Selector-Namespaced-Namespace*
                          (Selector-Namespaced-Namespace Selector-Namespaced-Namespace*))
                        (emit "|")
                        (Selector-Namespaced-Name Selector-Namespaced-Name*)])
  (Selector-Namespaced-Namespace
   : Selector-Namespaced-Namespace (Selector-Namespaced-Namespace*) -> * ()
   [,Symbol* (emit (~a Symbol*))])
  (Selector-Namespaced-Name
   : Selector-Namespaced-Name (Selector-Namespaced-Name*) -> * ()
   [,Symbol* (emit (~a Symbol*))])
  (Selector-Combination
   : Selector-Combination (Selector-Combination*) -> * ()
   [(selector/combination ,Selector-Combination-Combinator* (,Selector* ...))
    (emit
     (string-join
      (for/list ([selector (in-list Selector*)])
        (collect (Selector selector)))
      (collect (Selector-Combination-Combinator Selector-Combination-Combinator*))))])
  (Selector-Combination-Combinator
   : Selector-Combination-Combinator (Selector-Combination-Combinator*) -> * ()
   [,Selector-Combination-Combinator-Literal*
    (Selector-Combination-Combinator-Literal Selector-Combination-Combinator-Literal*)]
   [,Selector-Combination-Combinator-Reference*
    (Selector-Combination-Combinator-Reference Selector-Combination-Combinator-Reference*)])
  (Selector-Combination-Combinator-Literal
   : Selector-Combination-Combinator-Literal (Selector-Combination-Combinator-Literal*) -> * ()
   [,Symbols/Selector-Combination-Combinator-Literal*
    (emit (~a Symbols/Selector-Combination-Combinator-Literal*))])
  (Selector-Combination-Combinator-Reference
   : Selector-Combination-Combinator-Reference (Selector-Combination-Combinator-Reference*) -> * ()
   [(selector/combination/combinator/reference ,Symbol*)
    (emit " /") (emit (~a Symbol*)) (emit "/ ")])
  (Selector-Prefixed
   : Selector-Prefixed (Selector-Prefixed*) -> * ()
   [(selector/prefixed ,Selector-Prefixed-Prefix* ,Selector* ,Selector-Prefixed-Subject*)
    (when Selector* (Selector Selector*))
    (Selector-Prefixed-Prefix Selector-Prefixed-Prefix*)
    (Selector-Prefixed-Subject Selector-Prefixed-Subject*)])
  (Selector-Prefixed-Prefix
   : Selector-Prefixed-Prefix (Selector-Prefixed-Prefix*) -> * ()
   [,Symbols/Selector-Prefixed-Prefix* (emit (~a Symbols/Selector-Prefixed-Prefix*))])
  (Selector-Prefixed-Subject
   : Selector-Prefixed-Subject (Selector-Prefixed-Subject*) -> * ()
   [,Selector-Prefixed-Subject-Name* (Selector-Prefixed-Subject-Name Selector-Prefixed-Subject-Name*)]
   [,Selector-FunctionApplication* (Selector-FunctionApplication Selector-FunctionApplication*)])
  (Selector-Prefixed-Subject-Name
   : Selector-Prefixed-Subject-Name (Selector-Prefixed-Subject-Name*) -> * ()
   [,Symbol* (emit (~a Symbol*))])
  (Selector-FunctionApplication
   : Selector-FunctionApplication (Selector-FunctionApplication*) -> * ()
   [(selector/function-application ,Selector-FunctionApplication-Function*
                                   (,Selector-FunctionApplication-Argument* ...))
    (Selector-FunctionApplication-Function Selector-FunctionApplication-Function*)
    (emit "(")
    (emit
     (string-join
      (for/list ([selector/function-application/argument
                  (in-list Selector-FunctionApplication-Argument*)])
        (collect (Selector-FunctionApplication-Argument selector/function-application/argument)))
      ","))
    (emit ")")])
  (Selector-FunctionApplication-Function
   : Selector-FunctionApplication-Function (Selector-FunctionApplication-Function*) -> * ()
   [,Symbol* (emit (~a Symbol*))])
  (Selector-FunctionApplication-Argument
   : Selector-FunctionApplication-Argument (Selector-FunctionApplication-Argument*) -> * ()
   [,Selector* (Selector Selector*)]
   [,Selector-FunctionApplication* (Selector-FunctionApplication Selector-FunctionApplication*)]
   [,Selector-FunctionApplication-Argument-An+B*
    (Selector-FunctionApplication-Argument-An+B Selector-FunctionApplication-Argument-An+B*)])
  (Selector-FunctionApplication-Argument-An+B
   : Selector-FunctionApplication-Argument-An+B (Selector-FunctionApplication-Argument-An+B*) -> * ()
   [,Selector-FunctionApplication-Argument-An+B-Literal*
    (Selector-FunctionApplication-Argument-An+B-Literal
     Selector-FunctionApplication-Argument-An+B-Literal*)]
   [,Selector-FunctionApplication-Argument-An+B-Proper*
    (Selector-FunctionApplication-Argument-An+B-Proper
     Selector-FunctionApplication-Argument-An+B-Proper*)])
  (Selector-FunctionApplication-Argument-An+B-Literal
   : Selector-FunctionApplication-Argument-An+B-Literal
   (Selector-FunctionApplication-Argument-An+B-Literal*) -> * ()
   [,Symbols/Selector-FunctionApplication-Argument-An+B-Literal*
    (emit (~a Symbols/Selector-FunctionApplication-Argument-An+B-Literal*))]
   [,Integer* (emit (~a Integer*))])
  (Selector-FunctionApplication-Argument-An+B-Proper
   : Selector-FunctionApplication-Argument-An+B-Proper
   (Selector-FunctionApplication-Argument-An+B-Proper*) -> * ()
   [(selector/function-application/argument/An+B/proper
     ,Selector-FunctionApplication-Argument-An+B-Proper-Step*
     ,Selector-FunctionApplication-Argument-An+B-Proper-Offset*)
    (when Selector-FunctionApplication-Argument-An+B-Proper-Step*
      (Selector-FunctionApplication-Argument-An+B-Proper-Step
       Selector-FunctionApplication-Argument-An+B-Proper-Step*))
    (emit "n")
    (when Selector-FunctionApplication-Argument-An+B-Proper-Offset*
      (emit "+")
      (Selector-FunctionApplication-Argument-An+B-Proper-Offset
       Selector-FunctionApplication-Argument-An+B-Proper-Offset*))])
  (Selector-FunctionApplication-Argument-An+B-Proper-Step
   : Selector-FunctionApplication-Argument-An+B-Proper-Step
   (Selector-FunctionApplication-Argument-An+B-Proper-Step*) -> * ()
   [,Integer* (emit (~a Integer*))])
  (Selector-FunctionApplication-Argument-An+B-Proper-Offset
   : Selector-FunctionApplication-Argument-An+B-Proper-Offset
   (Selector-FunctionApplication-Argument-An+B-Proper-Offset*) -> * ()
   [,Integer* (emit (~a Integer*))])
  (Selector-Attribute
   : Selector-Attribute (Selector-Attribute*) -> * ()
   [(selector/attribute ,Selector* ,Selector-Attribute-Subject*)
    (when Selector* (Selector Selector*))
    (emit "[")
    (Selector-Attribute-Subject Selector-Attribute-Subject*)
    (emit "]")])
  (Selector-Attribute-Subject
   : Selector-Attribute-Subject (Selector-Attribute-Subject*) -> * ()
   [,Selector-Attribute-Subject-Literal*
    (Selector-Attribute-Subject-Literal Selector-Attribute-Subject-Literal*)]
   [,Selector-Namespaced* (Selector-Namespaced Selector-Namespaced*)]
   [,Selector-Attribute-Operation* (Selector-Attribute-Operation Selector-Attribute-Operation*)])
  (Selector-Attribute-Subject-Literal
   : Selector-Attribute-Subject-Literal (Selector-Attribute-Subject-Literal*) -> * ()
   [,Symbol* (emit (~a Symbol*))])
  (Selector-Attribute-Operation
   : Selector-Attribute-Operation (Selector-Attribute-Operation*) -> * ()
   [(selector/attribute/operation ,Selector-Attribute-Operation-Operator*
                                  ,Selector-Attribute-Operation-Operand1
                                  ,Selector-Attribute-Operation-Operand2)
    (Selector-Attribute-Operation-Operand Selector-Attribute-Operation-Operand1)
    (Selector-Attribute-Operation-Operator Selector-Attribute-Operation-Operator*)
    (Selector-Attribute-Operation-Operand Selector-Attribute-Operation-Operand2)])
  (Selector-Attribute-Operation-Operator
   : Selector-Attribute-Operation-Operator (Selector-Attribute-Operation-Operator*) -> * ()
   [,Symbols/Selector-Attribute-Operation-Operator*
    (emit (~a Symbols/Selector-Attribute-Operation-Operator*))])
  (Selector-Attribute-Operation-Operand
   : Selector-Attribute-Operation-Operand (Selector-Attribute-Operation-Operand*) -> * ()
   [,Selector-Attribute-Operation-Operand-Literal*
    (Selector-Attribute-Operation-Operand-Literal Selector-Attribute-Operation-Operand-Literal*)]
   [,Selector-Attribute-Operation-Operand-CaseInsensitive*
    (Selector-Attribute-Operation-Operand-CaseInsensitive
     Selector-Attribute-Operation-Operand-CaseInsensitive*)])
  (Selector-Attribute-Operation-Operand-Literal
   : Selector-Attribute-Operation-Operand-Literal
   (Selector-Attribute-Operation-Operand-Literal*) -> * ()
   [,Symbol* (emit (~a Symbol*))]
   [,String* (emit "\"") (emit String*) (emit "\"")])
  (Selector-Attribute-Operation-Operand-CaseInsensitive
   : Selector-Attribute-Operation-Operand-CaseInsensitive
   (Selector-Attribute-Operation-Operand-CaseInsensitive*) -> * ()
   [(selector/attribute/operation/operand/case-insensitive ,String*)
    (emit "\"") (emit String*) (emit "\" i")])

  (collect (Stylesheet Stylesheet*)))

(module+ test
  ;; stylesheet
  (check-equal? (css-expr->css (css-expr)) "")
  (check-equal? (css-expr->css (css-expr [body #:background-color black]
                                         [p #:background-color black]))
                "body{background-color:black;}p{background-color:black;}")

  ;; qualified-rule
  (check-equal? (css-expr->css (css-expr [body #:background-color black]))
                "body{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [body p #:background-color black]))
                "body,p{background-color:black;}")

  ;; at-rule
  (check-equal? (css-expr->css (css-expr [@ import "styles.css"])) "@import \"styles.css\";")
  (check-equal? (css-expr->css (css-expr [@ import ((apply url "http://example.com") something)]))
                "@import url(\"http://example.com\") something;")
  (check-equal? (css-expr->css (css-expr [@ media (and all (#:min-width (px 500)))]))
                "@media all and (min-width:500px);")
  (check-equal? (css-expr->css (css-expr [@ media (and all (#:min-width (px 500)))
                                            [body #:background-color black]]))
                "@media all and (min-width:500px){body{background-color:black;}}")
  (check-equal? (css-expr->css (css-expr [@ media (or all (#:min-width (px 500)))
                                            [body #:background-color black]]))
                "@media all or (min-width:500px){body{background-color:black;}}")
  (check-equal? (css-expr->css (css-expr [@ media (not (and all (#:min-width (px 500))))
                                            [body #:background-color black]]))
                "@media not all and (min-width:500px){body{background-color:black;}}")
  (check-equal? (css-expr->css (css-expr [@ media (only (and all (#:min-width (px 500))))
                                            [body #:background-color black]]))
                "@media only all and (min-width:500px){body{background-color:black;}}")
  (check-equal? (css-expr->css (css-expr [@ media screen print
                                            [body #:background-color black]]))
                "@media screen,print{body{background-color:black;}}")

  ;; declaration
  (check-equal? (css-expr->css (css-expr [body #:background-color black]))
                "body{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [body #:font-family "Helvetica" "Georgia"]))
                "body{font-family:\"Helvetica\",\"Georgia\";}")
  (check-equal? (css-expr->css (css-expr [body #:background-color black !important]))
                "body{background-color:black !important;}")

  ;; value
  (check-equal? (css-expr->css (css-expr [body #:font (/ 16px 1.2)]))
                "body{font:16px/1.2;}")
  (check-equal? (css-expr->css (css-expr [body #:font-size (px 16)]))
                "body{font-size:16px;}")
  (check-equal? (css-expr->css (css-expr [body #:font (/ (px 16) 1.2)]))
                "body{font:16px/1.2;}")
  (check-equal? (css-expr->css (css-expr [body #:text-shadow ((em .03) 0 black)]))
                "body{text-shadow:0.03em 0 black;}")

  ;; value/function-application
  (check-equal? (css-expr->css (css-expr [body #:margin-left (apply calc 2)]))
                "body{margin-left:calc(2);}")
  (check-equal? (css-expr->css (css-expr [body #:background-color (apply rgb 255 255 255 0)]))
                "body{background-color:rgb(255,255,255,0);}")

  ;; value/operator
  (check-equal? (css-expr->css (css-expr [body #:margin-left (apply calc (+ 3 2))]))
                "body{margin-left:calc(3 + 2);}")
  (check-equal? (css-expr->css (css-expr [body #:margin-left (apply calc (- 3 2))]))
                "body{margin-left:calc(3 - 2);}")
  (check-equal? (css-expr->css (css-expr [body #:margin-left (apply calc (* 3 2))]))
                "body{margin-left:calc(3*2);}")
  (check-equal? (css-expr->css (css-expr [body #:margin-left (apply calc (/ 3 2))]))
                "body{margin-left:calc(3/2);}")

  ;; value/unit
  (check-equal? (css-expr->css (css-expr [body #:margin-left (% 15)]))
                "body{margin-left:15%;}")
  (check-equal? (css-expr->css (css-expr [body #:margin-left (em 2)]))
                "body{margin-left:2em;}")
  ; omit rest of units since they are many and all equivalent

  ;; selector
  (check-equal? (css-expr->css (css-expr [(+ body p) #:background-color black]))
                "body+p{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(body p) #:background-color black]))
                "body p{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(> (body p) a) #:background-color black]))
                "body p>a{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(> (body p) (a span)) #:background-color black]))
                "body p>a span{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(body (> p a) span) #:background-color black]))
                "body p>a span{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(: (> p a) hover) #:background-color black]))
                "p>a:hover{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(\. div pull-quote) #:background-color black]))
                "div.pull-quote{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(\. pull-quote) #:background-color black]))
                ".pull-quote{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(attribute a (= href "https://example.com"))
                                          #:background-color black]))
                "a[href=\"https://example.com\"]{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(attribute (= href "https://example.com"))
                                          #:background-color black]))
                "[href=\"https://example.com\"]{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [em #:background-color black]))
                "em{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [* #:background-color black]))
                "*{background-color:black;}")

  ;; selector/combinator
  (check-equal? (css-expr->css (css-expr [(+ body p) #:background-color black]))
                "body+p{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(> body p) #:background-color black]))
                "body>p{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(~ body p) #:background-color black]))
                "body~p{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [((// for) body p) #:background-color black]))
                "body /for/ p{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(\|\| body p) #:background-color black]))
                "body||p{background-color:black;}")

  ;; selector/prefix
  (check-equal? (css-expr->css (css-expr [(\. div pull-quote) #:background-color black]))
                "div.pull-quote{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(\. pull-quote) #:background-color black]))
                ".pull-quote{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(\# picture-1) #:background-color black]))
                "#picture-1{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(: active) #:background-color black]))
                ":active{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(:: before) #:background-color black]))
                "::before{background-color:black;}")

  ;; selector/function-application
  (check-equal? (css-expr->css (css-expr [(: (apply nth-child 2)) #:background-color black]))
                ":nth-child(2){background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(: (apply something-else 2 3)) #:background-color black]))
                ":something-else(2,3){background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(: (apply something-else (apply calc 2) 3))
                                          #:background-color black]))
                ":something-else(calc(2),3){background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(: (apply something-else n))
                                          #:background-color black]))
                ":something-else(n){background-color:black;}")

  ;; An+B
  (check-equal? (css-expr->css (css-expr [(: (apply nth-child odd)) #:background-color black]))
                ":nth-child(odd){background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(: (apply nth-child even)) #:background-color black]))
                ":nth-child(even){background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(: (apply nth-child 5)) #:background-color black]))
                ":nth-child(5){background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(: (apply nth-child n)) #:background-color black]))
                ":nth-child(n){background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(: (apply nth-child (n 2 1))) #:background-color black]))
                ":nth-child(2n+1){background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(: (apply nth-child (n+ 4))) #:background-color black]))
                ":nth-child(n+4){background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(: (apply nth-child (n 2))) #:background-color black]))
                ":nth-child(2n){background-color:black;}")

  ;; selector/attribute
  (check-equal? (css-expr->css (css-expr [(attribute href) #:background-color black]))
                "[href]{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(attribute a href) #:background-color black]))
                "a[href]{background-color:black;}")
  (check-equal? (css-expr->css
                 (css-expr [(attribute (attribute a href) alt) #:background-color black]))
                "a[href][alt]{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(attribute a (= href "https://example.com"))
                                          #:background-color black]))
                "a[href=\"https://example.com\"]{background-color:black;}")

  ;; selector/attribute/operator
  (check-equal? (css-expr->css (css-expr [(attribute (= href "https://example.com"))
                                          #:background-color black]))
                "[href=\"https://example.com\"]{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(attribute (~= href "https://example.com"))
                                          #:background-color black]))
                "[href~=\"https://example.com\"]{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(attribute (^= href "https://example.com"))
                                          #:background-color black]))
                "[href^=\"https://example.com\"]{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(attribute ($= href "https://example.com"))
                                          #:background-color black]))
                "[href$=\"https://example.com\"]{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(attribute (*= href "https://example.com"))
                                          #:background-color black]))
                "[href*=\"https://example.com\"]{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(attribute (\|= href "https://example.com"))
                                          #:background-color black]))
                "[href|=\"https://example.com\"]{background-color:black;}")

  ;; selector/attribute/value
  (check-equal? (css-expr->css (css-expr
                                [(attribute (= href (case-insensitive "https://example.com")))
                                 #:background-color black]))
                "[href=\"https://example.com\" i]{background-color:black;}")

  ;; identifier
  (check-equal? (css-expr->css (css-expr [body #:background-color black]))
                "body{background-color:black;}")

  ;; identifier/namespaced
  (check-equal? (css-expr->css (css-expr [(\| ns body) #:background-color black]))
                "ns|body{background-color:black;}")
  (check-equal? (css-expr->css (css-expr [(\| body) #:background-color black]))
                "|body{background-color:black;}")

  ;; basic values
  (check-equal? (css-expr->css (css-expr [body #:line-height 1.3]))
                "body{line-height:1.3;}")
  (check-equal? (css-expr->css (css-expr [@ import "styles.css"])) "@import \"styles.css\";")
  (check-equal? (css-expr->css (css-expr [body #:background-color \#ffffff]))
                "body{background-color:#ffffff;}")
  (check-equal? (css-expr->css (css-expr [body #:background-color |#ffffff|]))
                "body{background-color:#ffffff;}")

  ;; `http://bettermotherfuckingwebsite.com/'
  (check-equal? (css-expr->css (css-expr [body
                                          #:margin ((px 40) auto)
                                          #:max-width (px 650)
                                          #:line-height 1.6
                                          #:font-size (px 18)
                                          #:color |#444|
                                          #:padding (0 (px 10))]
                                         [h1 h2 h3
                                             #:line-height 1.2]))
                (~a "body{margin:40px auto;max-width:650px;line-height:1.6;font-size:18px;"
                    "color:#444;padding:0 10px;}h1,h2,h3{line-height:1.2;}")))

;; ---------------------------------------------------------------------------------------------------

(define (unnest element-of-interest? nested-elements unnest-element make-remaining-rule)
  (define-values (nested-elements-of-interest other-nested-elements)
    (partition element-of-interest? nested-elements))
  (define unnested-elements-of-interest
    (for/list ([nested-element-of-interest (in-list nested-elements-of-interest)])
      (unnest-element nested-element-of-interest)))
  (if (empty? other-nested-elements)
      unnested-elements-of-interest
      `(,(make-remaining-rule other-nested-elements)
        ,@unnested-elements-of-interest)))

;; ---------------------------------------------------------------------------------------------------

(define-language css/with-nested-qualified-rules
  (extends css/core)
  (QualifiedRule-Element (QualifiedRule-Element) (+ QualifiedRule))
  (Selector (Selector) (+ Selector-Parent))
  (Selector-Parent (Selector-Parent) (+ (selector/parent (maybe Selector-Parent-Suffix))))
  (Selector-Parent-Suffix (Selector-Parent-Suffix) (+ Symbol)))

(define-pass flatten/qualified-rules : css/with-nested-qualified-rules (Stylesheet*) -> css/core ()
  (definitions
    (define (nested-qualified-rule? nested-rule)
      (nanopass-case
       (css/with-nested-qualified-rules QualifiedRule) nested-rule
       [(qualified-rule (,Selector* ...) (,QualifiedRule-Element* ...)) #t]
       [else #f]))
    (define (merge-selectors selector/parent selector/child)
      (define-pass traverse/child
        : (css/with-nested-qualified-rules Selector) (Selector*)
        -> (css/with-nested-qualified-rules Selector) ()
        (Selector-Parent
         : Selector-Parent (Selector-Parent*) -> Selector ()
         [(selector/parent ,Selector-Parent-Suffix*)
          (if Selector-Parent-Suffix*
              (traverse/parent selector/parent Selector-Parent-Suffix*)
              selector/parent)]))
      (define-pass traverse/parent
        : (css/with-nested-qualified-rules Selector) (Selector* suffix)
        -> (css/with-nested-qualified-rules Selector) ()
        (definitions
          (define (append-suffix symbol)
            (string->symbol (~a symbol "-" suffix)))
          (define (unsupported)
            (raise-user-error
             (~a "parent selector is not supported on child selector;\n"
                 " parent: " (unparse-css/with-nested-qualified-rules selector/parent) "\n"
                 " child: " (unparse-css/with-nested-qualified-rules selector/child)))))
        (Selector : Selector (Selector*) -> Selector ())
        (Selector-Tag : Selector-Tag (Selector-Tag*) -> Selector-Tag ()
                      [,Symbol* (append-suffix Symbol*)])
        (Selector-Namespaced
         : Selector-Namespaced (Selector-Namespaced*) -> Selector-Namespaced ()
         [(selector/namespaced ,[Selector-Namespaced-Namespace*] ,[Selector-Namespaced-Name*])
          `(selector/namespaced ,Selector-Namespaced-Namespace*
                                ,(append-suffix Selector-Namespaced-Name*))])
        (Selector-Combination
         : Selector-Combination (Selector-Combination*) -> Selector-Combination ()
         [(selector/combination ,[Selector-Combination-Combinator*] (,Selector* ...))
          `(selector/combination ,Selector-Combination-Combinator*
                                 (,(drop-right Selector* 1) ... ,(Selector (last Selector*))))])
        (Selector-Prefixed
         : Selector-Prefixed (Selector-Prefixed*) -> Selector-Prefixed ()
         [(selector/prefixed ,[Selector-Prefixed-Prefix*] ,Selector* ,[Selector-Prefixed-Subject*])
          `(selector/prefixed ,Selector-Prefixed-Prefix* ,Selector*
                              ,(append-suffix Selector-Prefixed-Subject*))])
        (Selector-FunctionApplication
         : Selector-FunctionApplication (Selector-FunctionApplication*)
         -> Selector-FunctionApplication ()
         [else (unsupported)])
        (Selector-Attribute : Selector-Attribute (Selector-Attribute*) -> Selector-Attribute ()
                            [else (unsupported)]))
      (traverse/child selector/child)))

  (Stylesheet
   : Stylesheet (Stylesheet*) -> Stylesheet ()
   [(stylesheet (,Rule* ...)) `(stylesheet (,(append-map Rule Rule*) ...))])
  (Rule : Rule (Rule*) -> * ()
        [,QualifiedRule* (QualifiedRule QualifiedRule*)]
        [,AtRule* (AtRule AtRule*)])
  (QualifiedRule
   : QualifiedRule (QualifiedRule*) -> * ()
   [(qualified-rule (,Selector1 ...) (,QualifiedRule-Element* ...))
    (apply
     append
     (unnest
      nested-qualified-rule?
      QualifiedRule-Element*
      (λ (nested-qualified-rule)
        (nanopass-case
         (css/with-nested-qualified-rules QualifiedRule) nested-qualified-rule
         [(qualified-rule (,Selector2 ...) (,QualifiedRule-Element* ...))
          (QualifiedRule
           (with-output-language (css/with-nested-qualified-rules QualifiedRule)
             `(qualified-rule
               (,(for/list ([selectors (in-list (cartesian-product Selector1 Selector2))])
                   (apply merge-selectors selectors))
                ...)
               (,QualifiedRule-Element* ...))))]))
      (λ (other-nested-elements)
        `(,(with-output-language (css/core QualifiedRule)
             `(qualified-rule (,(map Selector Selector1) ...)
                              (,(append-map QualifiedRule-Element other-nested-elements) ...)))))))])
  (QualifiedRule-Element : QualifiedRule-Element (QualifiedRule-Element*) -> * ()
                         [,QualifiedRule* (QualifiedRule QualifiedRule*)]
                         [,Declaration* (Declaration Declaration*)])
  (AtRule
   : AtRule (AtRule*) -> * ()
   [(at-rule ,AtRule-Name* (,[AtRule-Expression*] ...) (,AtRule-Element* ...))
    `(,(with-output-language (css/core AtRule)
         `(at-rule ,AtRule-Name* (,AtRule-Expression* ...)
                   (,(append-map AtRule-Element AtRule-Element*) ...))))])
  (AtRule-Element : AtRule-Element (AtRule-Element*) -> * ()
                  [,QualifiedRule* (QualifiedRule QualifiedRule*)]
                  [,Declaration* (Declaration Declaration*)])
  (Declaration : Declaration (Declaration*) -> * ()
               [(declaration ,[Declaration-Name*] (,[Value*] ...) ,[Declaration-Important*])
                `(,(with-output-language (css/core Declaration)
                     `(declaration ,Declaration-Name* (,Value* ...) ,Declaration-Important*)))])
  (Selector : Selector (Selector*) -> Selector ()))

(define-pass normalize-nested-selectors
  : css/with-nested-qualified-rules (Stylesheet*) -> css/with-nested-qualified-rules ()

  (QualifiedRule-Element
   : QualifiedRule-Element (QualifiedRule-Element*) -> QualifiedRule-Element ()
   [(qualified-rule (,[Selector*] ...) (,[QualifiedRule-Element*] ...))
    `(qualified-rule
      (,(with-output-language (css/with-nested-qualified-rules Selector)
          (for/list ([selector (in-list Selector*)])
            (if (includes-parent-reference? selector)
                selector
                `(selector/combination | | ((selector/parent #f) ,selector)))))
       ...)
      (,QualifiedRule-Element* ...))]))

(define-pass no-selector-parent-on-top-level!
  : css/with-nested-qualified-rules (Stylesheet*) -> css/with-nested-qualified-rules ()
  (QualifiedRule
   : QualifiedRule (QualifiedRule*) -> QualifiedRule ()
   [(qualified-rule (,Selector* ...) (,QualifiedRule-Element* ...))
    `(qualified-rule
      (,(for/list ([selector (in-list Selector*)])
          (when (includes-parent-reference? selector)
            (raise-user-error
             (~a "reference to parent selector found in selector at top level;\n"
                 " only nested qualified rules can refer to parents\n"
                 " rule: " (unparse-css/with-nested-qualified-rules QualifiedRule*))))
          selector)
       ...)
      (,QualifiedRule-Element* ...))]))

(define (includes-parent-reference? selector)
  (define current-parent-reference? (make-parameter #f))
  (define-pass traverse
    : (css/with-nested-qualified-rules Selector) (Selector*)
    -> (css/with-nested-qualified-rules Selector) ()
    (Selector-Parent : Selector-Parent (Selector-Parent*) -> Selector-Parent ()
                     [else (current-parent-reference? #t) Selector-Parent*]))
  (traverse selector)
  (current-parent-reference?))

(module+ test
  (check-equal? (css-expr->css (css-expr [body [p #:background-color |#ffffff|]]))
                "body p{background-color:#ffffff;}")
  (check-equal? (css-expr->css (css-expr [body article [p div #:background-color |#ffffff|]]))
                "body p,body div,article p,article div{background-color:#ffffff;}")
  (check-equal? (css-expr->css (css-expr [body #:background-color black
                                               [p #:background-color |#ffffff|]]))
                "body{background-color:black;}body p{background-color:#ffffff;}")
  (check-equal? (css-expr->css (css-expr [body [p [a #:background-color |#ffffff|]]]))
                "body p a{background-color:#ffffff;}")
  (check-equal? (css-expr->css (css-expr [a [(: & hover) #:background-color |#ffffff|]]))
                "a:hover{background-color:#ffffff;}")
  (check-equal? (css-expr->css (css-expr [body [(&- sidebar) #:background-color |#ffffff|]]))
                "body-sidebar{background-color:#ffffff;}")
  (check-equal? (css-expr->css (css-expr [(\| ns body) [(&- sidebar) #:background-color |#ffffff|]]))
                "ns|body-sidebar{background-color:#ffffff;}")
  (check-equal? (css-expr->css (css-expr [(+ head body) [(&- sidebar) #:background-color |#ffffff|]]))
                "head+body-sidebar{background-color:#ffffff;}")
  (check-equal? (css-expr->css (css-expr [(\# main) [(&- sidebar) #:background-color |#ffffff|]]))
                "#main-sidebar{background-color:#ffffff;}")
  (check-exn exn:fail:user?
             (λ ()
               (css-expr->css (css-expr [(&- sidebar) #:background-color |#ffffff|]))))
  (check-exn exn:fail:user?
             (λ ()
               (css-expr->css (css-expr [(attribute main)
                                         [(&- sidebar) #:background-color |#ffffff|]]))))
  (check-exn exn:fail:user?
             (λ ()
               (css-expr->css (css-expr [(: (apply nth-child 2))
                                         [(&- sidebar) #:background-color |#ffffff|]]))))
  (check-equal? (css-expr->css (css-expr [p [span #:background-color pink]
                                            [a #:background-color black]]))
                "p span{background-color:pink;}p a{background-color:black;}")
  ;; The following reordering of declarations is weird to me, but is how Sass behaves.
  (check-equal? (css-expr->css (css-expr [body
                                          #:margin 0
                                          [p #:background-color |#ffffff|]
                                          #:padding 0]))
                "body{margin:0;padding:0;}body p{background-color:#ffffff;}"))

;; ---------------------------------------------------------------------------------------------------

(define-language css/with-nested-at-rules
  (extends css/with-nested-qualified-rules)
  (QualifiedRule-Element (Rule-Element) (+ AtRule))
  (AtRule-Element (AtRule-Element) (+ AtRule)))

(define-pass flatten/at-rules
  : css/with-nested-at-rules (Stylesheet*) -> css/with-nested-qualified-rules ()
  (definitions
    (define (nested-at-rule? nested-rule)
      (nanopass-case
       (css/with-nested-qualified-rules AtRule) nested-rule
       [(at-rule ,AtRule-Name* (,AtRule-Expression* ...) (,AtRule-Element* ...)) #t]
       [else #f])))

  (Stylesheet : Stylesheet (Stylesheet*) -> Stylesheet ()
              [(stylesheet (,Rule* ...)) `(stylesheet (,(append-map Rule Rule*) ...))])
  (Rule : Rule (Rule*) -> * ()
        [,QualifiedRule* (QualifiedRule QualifiedRule*)]
        [,AtRule* (AtRule AtRule*)])
  (QualifiedRule
   : QualifiedRule (QualifiedRule*) -> * ()
   [(qualified-rule (,[Selector*] ...) (,QualifiedRule-Element* ...))
    (unnest
     nested-at-rule?
     (append-map QualifiedRule-Element QualifiedRule-Element*)
     (λ (nested-at-rule)
       (nanopass-case
        (css/with-nested-qualified-rules AtRule) nested-at-rule
        [(at-rule ,AtRule-Name* (,AtRule-Expression* ...) (,AtRule-Element* ...))
         (with-output-language (css/with-nested-qualified-rules AtRule)
           `(at-rule ,AtRule-Name* (,AtRule-Expression* ...)
                     ((qualified-rule (,Selector* ...) (,AtRule-Element* ...)))))]))
     (λ (other-nested-elements)
       (with-output-language (css/with-nested-qualified-rules QualifiedRule)
         `(qualified-rule (,Selector* ...) (,other-nested-elements ...)))))])
  (QualifiedRule-Element : QualifiedRule-Element (QualifiedRule-Element*) -> * ()
                         [,QualifiedRule* (QualifiedRule QualifiedRule*)]
                         [,AtRule* (AtRule AtRule*)]
                         [,Declaration* (Declaration Declaration*)])
  (AtRule
   : AtRule (AtRule*) -> * ()
   [(at-rule ,[AtRule-Name*] (,[AtRule-Expression*] ...) ())
    `(,(with-output-language (css/with-nested-qualified-rules AtRule)
         `(at-rule ,AtRule-Name* (,AtRule-Expression* ...) ())))]
   [(at-rule ,[AtRule-Name1] (,[AtRule-Expression1] ...) (,AtRule-Element* ...))
    (unnest
     nested-at-rule?
     (append-map AtRule-Element AtRule-Element*)
     (λ (nested-at-rule)
       (nanopass-case
        (css/with-nested-qualified-rules AtRule) nested-at-rule
        [(at-rule ,AtRule-Name2 (,AtRule-Expression2 ...) (,AtRule-Element* ...))
         (guard (equal? AtRule-Name1 AtRule-Name2))
         (define at-rule/expressions
           (for/list ([at-rule/expressions
                       (in-list (cartesian-product AtRule-Expression1 AtRule-Expression2))])
             (with-output-language (css/with-nested-qualified-rules AtRule-Expression)
               `(at-rule/expression/operation/n-ary and (,at-rule/expressions ...)))))
         (with-output-language (css/with-nested-qualified-rules AtRule)
           `(at-rule ,AtRule-Name1 (,at-rule/expressions ...) (,AtRule-Element* ...)))]
        [else nested-at-rule]))
     (λ (other-nested-elements)
       (with-output-language (css/with-nested-qualified-rules AtRule)
         `(at-rule ,AtRule-Name1 (,AtRule-Expression1 ...) (,other-nested-elements ...)))))])
  (AtRule-Element : AtRule-Element (AtRule-Element*) -> * ()
                  [,QualifiedRule* (QualifiedRule QualifiedRule*)]
                  [,AtRule* (AtRule AtRule*)]
                  [,Declaration* (Declaration Declaration*)])
  (Declaration : Declaration (Declaration*) -> * ()
               [(declaration ,[Declaration-Name*] (,[Value*] ...) ,[Declaration-Important*])
                `(,(with-output-language (css/with-nested-qualified-rules Declaration)
                     `(declaration ,Declaration-Name* (,Value* ...) ,Declaration-Important*)))]))

(module+ test
  (check-equal? (css-expr->css (css-expr [body #:background-color black
                                               [@ media screen #:background-color white]]))
                "body{background-color:black;}@media screen{body{background-color:white;}}")
  (check-equal? (css-expr->css (css-expr [@ media screen
                                            [body #:background-color black]
                                            [@ font-face #:source "font.ttf"]]))
                "@media screen{body{background-color:black;}}@font-face{source:\"font.ttf\";}")
  (check-equal? (css-expr->css (css-expr [@ media screen
                                            [@ media (#:min-width (px 320))
                                               [body #:background-color black]]]))
                "@media screen and (min-width:320px){body{background-color:black;}}")
  (check-equal? (css-expr->css (css-expr [@ media screen
                                            [body [@ media (#:min-width (px 320))
                                                     #:background-color black]]]))
                "@media screen and (min-width:320px){body{background-color:black;}}")
  (check-equal? (css-expr->css (css-expr [body [@ media screen
                                                  [p #:background-color black]]]))
                "@media screen{body p{background-color:black;}}")
  (check-equal? (css-expr->css (css-expr [body #:background-color black [@ import "styles.css"]]))
                "body{background-color:black;}@import \"styles.css\";")
  (check-equal? (css-expr->css (css-expr [@ media screen [body #:background-color black]
                                            [@ import "styles.css"]]))
                "@media screen{body{background-color:black;}}@import \"styles.css\";")
  (check-equal? (css-expr->css (css-expr [body #:background-color black
                                               [@ media screen #:background-color white]
                                               [@ import "styles.css"]]))
                (~a "body{background-color:black;}"
                    "@media screen{body{background-color:white;}}"
                    "@import \"styles.css\";"))
  (check-equal? (css-expr->css (css-expr [body #:background-color black
                                               [@ media screen #:background-color black]
                                               [@ media print #:background-color white]]))
                (~a "body{background-color:black;}"
                    "@media screen{body{background-color:black;}}"
                    "@media print{body{background-color:white;}}")))

;; ---------------------------------------------------------------------------------------------------

(define-language css/with-nested-declarations
  (extends css/with-nested-at-rules)
  (Declaration
   (Declaration)
   (- (declaration Declaration-Name (Value ...) Declaration-Important))
   (+ (declaration Declaration-Name (Value ...) Declaration-Important (Declaration ...)))))

(define-pass flatten/declarations
  : css/with-nested-declarations (Stylesheet*) -> css/with-nested-at-rules ()
  (QualifiedRule
   : QualifiedRule (QualifiedRule*) -> QualifiedRule ()
   [(qualified-rule (,[Selector*] ...) (,QualifiedRule-Element* ...))
    `(qualified-rule (,Selector* ...)
                     (,(append-map QualifiedRule-Element QualifiedRule-Element*) ...))])
  (QualifiedRule-Element
   : QualifiedRule-Element (QualifiedRule-Element*) -> * ()
   [,QualifiedRule* `(,(QualifiedRule QualifiedRule*))]
   [,AtRule* `(,(AtRule AtRule*))]
   [,Declaration* (Declaration Declaration*)])
  (AtRule
   : AtRule (AtRule*) -> AtRule ()
   [(at-rule ,[AtRule-Name*] (,[AtRule-Expression*] ...) (,AtRule-Element* ...))
    `(at-rule ,AtRule-Name* (,AtRule-Expression* ...)
              (,(append-map AtRule-Element AtRule-Element*) ...))])
  (AtRule-Expression
   : AtRule-Expression (AtRule-Expression*) -> AtRule-Expression ()
   [,Declaration*
    (define declarations (Declaration Declaration*))
    (if (= (length declarations) 1)
        (first declarations)
        `(at-rule/expression/operation/n-ary and (,declarations ...)))])
  (AtRule-Element
   : AtRule-Element (AtRule-Element*) -> * ()
   [,QualifiedRule* `(,(QualifiedRule QualifiedRule*))]
   [,AtRule* `(,(AtRule AtRule*))]
   [,Declaration* (Declaration Declaration*)])
  (Declaration
   : Declaration (Declaration*) -> * ()
   [(declaration ,Declaration-Name* () ,Declaration-Important* ()) empty]
   [(declaration ,[Declaration-Name*] (,[Value*] ...) ,[Declaration-Important*] ())
    `(,(with-output-language (css/with-nested-at-rules Declaration)
         `(declaration ,Declaration-Name* (,Value* ...) ,Declaration-Important*)))]
   [(declaration ,[Declaration-Name1] () ,[Declaration-Important*] (,Declaration* ...))
    (for/list ([declaration (in-list (append-map Declaration Declaration*))])
      (nanopass-case
       (css/with-nested-at-rules Declaration) declaration
       [(declaration ,Declaration-Name2 (,Value* ...) ,Declaration-Important*)
        (with-output-language (css/with-nested-at-rules Declaration)
          `(declaration ,(string->symbol (~a Declaration-Name1 "-" Declaration-Name2))
                        (,Value* ...) ,Declaration-Important*))]))]
   [(declaration ,Declaration-Name* (,Value* ...) ,Declaration-Important* (,Declaration* ...))
    (with-output-language (css/with-nested-declarations Declaration)
      (append
       (Declaration
        `(declaration ,Declaration-Name* (,Value* ...) ,Declaration-Important* ()))
       (Declaration
        `(declaration ,Declaration-Name* () ,Declaration-Important* (,Declaration* ...)))))]))

(module+ test
  (check-equal? (css-expr->css (css-expr [body #:background (#:color |#ffffff|)]))
                "body{background-color:#ffffff;}")
  (check-equal? (css-expr->css (css-expr [body
                                          #:background repeat-x (#:color |#ffffff|)]))
                "body{background:repeat-x;background-color:#ffffff;}")
  (check-equal? (css-expr->css (css-expr [body
                                          #:background repeat-x !important (#:color |#ffffff|)]))
                "body{background:repeat-x !important;background-color:#ffffff;}")
  (check-equal? (css-expr->css (css-expr [body
                                          #:background repeat-x (#:color |#ffffff| !important)]))
                "body{background:repeat-x;background-color:#ffffff !important;}")
  (check-equal? (css-expr->css (css-expr [body #:background (#:color |#ffffff| #:repeat repeat-x)]))
                "body{background-color:#ffffff;background-repeat:repeat-x;}")
  (check-equal? (css-expr->css (css-expr [body #:border (#:left (#:width thin))]))
                "body{border-left-width:thin;}")
  (check-equal? (css-expr->css (css-expr [body #:background (#:color |#ffffff|)
                                               #:border (#:left none)]))
                "body{background-color:#ffffff;border-left:none;}")
  (check-equal? (css-expr->css (css-expr [@ media (#:max (#:width 700px))
                                            [p #:background-color black]]))
                "@media (max-width:700px){p{background-color:black;}}")
  (check-equal? (css-expr->css (css-expr [@ media (#:max (#:width 700px #:height 600px))
                                            [p #:background-color black]]))
                "@media (max-width:700px) and (max-height:600px){p{background-color:black;}}"))

;; ---------------------------------------------------------------------------------------------------

(define-language css/full
  (extends css/with-nested-declarations))

(define-pass css/full->css/with-nested-declarations
  : css/full (Stylesheet*) -> css/with-nested-declarations ())

;; ---------------------------------------------------------------------------------------------------

(define-pass parse : * (Stylesheet*) -> css/full ()
  (definitions
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
      (pattern subject:selector/function-application))
    (define-syntax-class selector/function-application
      (pattern ((~datum apply) name:identifier argument:selector/function-application/argument ...+)))
    (define-syntax-class selector/function-application/argument
      (pattern argument:selector/function-application/argument/An+B)
      (pattern argument:selector/function-application)
      (pattern argument:selector))
    (define-syntax-class selector/function-application/argument/An+B
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
      (pattern (~datum \|\|))))

  (Stylesheet
   : * (Stylesheet*) -> Stylesheet ()
   (syntax-parse Stylesheet*
     [stylesheet:stylesheet
      `(stylesheet (,(map Rule (syntax->list #'(stylesheet.rule ...))) ...))]))
  (Rule
   : * (Rule*) -> Rule ()
   (syntax-parse Rule*
     [[(~datum @) name:identifier expression:at-rule/expression ...
                  block-element:block-element ...]
      `(at-rule ,(syntax->datum #'name)
                (,(map AtRule-Expression (syntax->list #'(expression ...))) ...)
                (,(map BlockElement (syntax->list #'(block-element ...))) ...))]
     [[selector:selector ...+ block-element:block-element ...+]
      `(qualified-rule (,(map Selector (syntax->list #'(selector ...))) ...)
                       (,(map BlockElement (syntax->list #'(block-element ...))) ...))]))
  (BlockElement
   : * (BlockElement*) -> * ()
   (syntax-parse BlockElement*
     [(element:declaration) (Declaration #'element)]
     [(element:rule) (Rule #'element)]))
  (AtRule-Expression
   : * (AtRule-Expression*) -> AtRule-Expression ()
   (syntax-parse AtRule-Expression*
     [(expression/declaration:declaration) (Declaration #'expression/declaration)]
     [(operator:at-rule/expression/operation/operator/unary operand:at-rule/expression)
      `(at-rule/expression/operation/unary
        ,(AtRule-Expression-Operation-Operator-Unary #'operator)
        ,(AtRule-Expression #'operand))]
     [(operator:at-rule/expression/operation/operator/n-ary operand:at-rule/expression ...+)
      `(at-rule/expression/operation/n-ary
        ,(AtRule-Expression-Operation-Operator-NAry #'operator)
        (,(map AtRule-Expression (syntax->list #'(operand ...))) ...))]
     [expression/value:value (Value #'expression/value)]
     [(expression:at-rule/expression ...+)
      `(at-rule/expression/operation/n-ary
        | |
        (,(map AtRule-Expression (syntax->list #'(expression ...))) ...))]))
  (AtRule-Expression-Operation-Operator-Unary
   : * (AtRule-Expression-Operation-Operator-Unary*) -> * ()
   (syntax-parse AtRule-Expression-Operation-Operator-Unary*
     [operator:at-rule/expression/operation/operator/unary (syntax->datum #'operator)]))
  (AtRule-Expression-Operation-Operator-NAry
   : * (AtRule-Expression-Operation-Operator-NAry*) -> * ()
   (syntax-parse AtRule-Expression-Operation-Operator-NAry*
     [operator:at-rule/expression/operation/operator/n-ary (syntax->datum #'operator)]))
  (Declaration
   : * (Declaration*) -> Declaration ()
   (syntax-parse Declaration*
     [(name:keyword value:value ...+
                    (~optional (~and (~datum !important) important?))
                    (~optional (nested-declaration:declaration ...+)))
      `(declaration ,(string->symbol (keyword->string (syntax->datum #'name)))
                    (,(map Value (syntax->list #'(value ...))) ...)
                    ,(and (attribute important?) #t)
                    (,(if (attribute nested-declaration)
                          (map Declaration (syntax->list #'(nested-declaration ...)))
                          '()) ...))]
     [(name:keyword (~optional (nested-declaration:declaration ...+)))
      `(declaration ,(string->symbol (keyword->string (syntax->datum #'name)))
                    ()
                    #f
                    (,(if (attribute nested-declaration)
                          (map Declaration (syntax->list #'(nested-declaration ...)))
                          '()) ...))]))
  (Value
   : * (Value*) -> Value ()
   (syntax-parse Value*
     [value/identifier:value/identifier (syntax->datum #'value/identifier)]
     [value/number:number (syntax->datum #'value/number)]
     [value/string:str (syntax->datum #'value/string)]
     [(unit:value/measurement/unit magnitude:number)
      `(value/measurement ,(Value-Measurement-Unit #'unit) ,(syntax->datum #'magnitude))]
     [((~datum apply) name:identifier argument:value ...+)
      `(value/function-application ,(syntax->datum #'name)
                                   (,(map Value (syntax->list #'(argument ...))) ...))]
     [(operator:value/operation/operator operand:value ...+)
      `(value-operation ,(Value-Operation-Operator #'operator)
                        (,(map Value (syntax->list #'(operand ...))) ...))]
     [(value/list:value ...+)
      `(value-operation | | (,(map Value (syntax->list #'(value/list ...))) ...))]))
  (Value-Measurement-Unit
   : * (Value-Measurement-Unit*) -> * ()
   (syntax-parse Value-Measurement-Unit*
     [unit:value/measurement/unit (syntax->datum #'unit)]))
  (Value-Operation-Operator
   : * (Value-Operation-Operator*) -> * ()
   (syntax-parse Value-Operation-Operator*
     [operator:value/operation/operator (syntax->datum #'operator)]))
  (Selector
   : * (Selector*) -> Selector ()
   (syntax-parse Selector*
     [(~datum &) `(selector/parent #f)]
     [((~datum &-) suffix:identifier) `(selector/parent ,(syntax->datum #'suffix))]
     [selector/namespaced:selector/namespaced (Selector-Namespaced #'selector/namespaced)]
     [(prefix:selector/prefixed/prefix (~optional selector:selector)
                                       subject:selector/prefixed/subject)
      `(selector/prefixed ,(Selector-Prefixed-Prefix #'prefix)
                          ,(and (attribute selector) (Selector #'selector))
                          ,(Selector-Prefixed-Subject #'subject))]
     [((~datum attribute) (~optional selector:selector) subject:selector/attribute/subject)
      `(selector/attribute ,(and (attribute selector) (Selector #'selector))
                           ,(Selector-Attribute-Subject #'subject))]
     [(combinator:selector/combination/combinator combinand:selector ...+)
      `(selector/combination ,(Selector-Combination-Combinator #'combinator)
                             (,(map Selector (syntax->list #'(combinand ...))) ...))]
     [(selector/list:selector ...+)
      `(selector/combination | |
                             (,(map Selector (syntax->list #'(selector/list ...))) ...))]
     [selector/identifier:identifier (syntax->datum #'selector/identifier)]))
  (Selector-Namespaced
   : * (Selector-Namespaced*) -> Selector-Namespaced ()
   (syntax-parse Selector-Namespaced*
     [selector:selector/namespaced
      `(selector/namespaced ,(and (attribute selector.namespace)
                                  (syntax->datum #'selector.namespace))
                            ,(syntax->datum #'selector.element))]))
  (Selector-Prefixed-Prefix
   : * (Selector-Prefixed-Prefix*) -> * ()
   (syntax-parse Selector-Prefixed-Prefix*
     [prefix:selector/prefixed/prefix (syntax->datum #'prefix)]))
  (Selector-Prefixed-Subject
   : * (Selector-Prefixed-Subject*) -> * ()
   (syntax-parse Selector-Prefixed-Subject*
     [subject:identifier (syntax->datum #'subject)]
     [subject:selector/function-application (Selector-FunctionApplication #'subject)]))
  (Selector-FunctionApplication
   : * (Selector-FunctionApplication*) -> Selector-FunctionApplication ()
   (syntax-parse Selector-FunctionApplication*
     [selector:selector/function-application
      `(selector/function-application
        ,(syntax->datum #'selector.name)
        (,(map Selector-FunctionApplication-Argument
               (syntax->list #'(selector.argument ...))) ...))]))
  (Selector-FunctionApplication-Argument
   : * (Selector-FunctionApplication-Argument*) -> * ()
   (syntax-parse Selector-FunctionApplication-Argument*
     [argument:selector/function-application/argument/An+B
      (Selector-FunctionApplication-Argument-An+B #'argument)]
     [argument:selector/function-application (Selector-FunctionApplication #'argument)]
     [argument:selector (Selector #'argument)]))
  (Selector-FunctionApplication-Argument-An+B
   : * (Selector-FunctionApplication-Argument-An+B*) -> Selector-FunctionApplication-Argument-An+B ()
   (syntax-parse Selector-FunctionApplication-Argument-An+B*
     [(~datum odd) `odd]
     [(~datum even) `even]
     [An+B:integer (syntax->datum #'An+B)]
     [(~datum n) `(selector/function-application/argument/An+B/proper #f #f)]
     [((~datum n) step:integer)
      `(selector/function-application/argument/An+B/proper ,(syntax->datum #'step) #f)]
     [((~datum n) step:integer offset:integer)
      `(selector/function-application/argument/An+B/proper ,(syntax->datum #'step)
                                                           ,(syntax->datum #'offset))]
     [((~datum n+) offset:integer)
      `(selector/function-application/argument/An+B/proper #f ,(syntax->datum #'offset))]))
  (Selector-Attribute-Subject
   : * (Selector-Attribute-Subject*) -> Selector-Attribute-Subject ()
   (syntax-parse Selector-Attribute-Subject*
     [subject:identifier (syntax->datum #'subject)]
     [subject:selector/namespaced (Selector-Namespaced #'subject)]
     [(operator:selector/attribute/operation/operator
       operand/left:selector/attribute/operation/operand
       operand/right:selector/attribute/operation/operand)
      `(selector/attribute/operation
        ,(Selector-Attribute-Operation-Operator #'operator)
        ,(Selector-Attribute-Operation-Operand #'operand/left)
        ,(Selector-Attribute-Operation-Operand #'operand/right))]))
  (Selector-Attribute-Operation-Operator
   : * (Selector-Attribute-Operation-Operator*) -> * ()
   (syntax-parse Selector-Attribute-Operation-Operator*
     [operator:selector/attribute/operation/operator (syntax->datum #'operator)]))
  (Selector-Attribute-Operation-Operand
   : * (Selector-Attribute-Operation-Operand*) -> Selector-Attribute-Operation-Operand ()
   (syntax-parse Selector-Attribute-Operation-Operand*
     [operand:identifier (syntax->datum #'operand)]
     [operand:str (syntax->datum #'operand)]
     [((~datum case-insensitive) string:str)
      `(selector/attribute/operation/operand/case-insensitive ,(syntax->datum #'string))]))
  (Selector-Combination-Combinator
   : * (Selector-Combination-Combinator*) -> Selector-Combination-Combinator ()
   (syntax-parse Selector-Combination-Combinator*
     [((~datum //) reference:identifier)
      `(selector/combination/combinator/reference ,(syntax->datum #'reference))]
     [combinator:selector/combination/combinator (syntax->datum #'combinator)]))

  (Stylesheet Stylesheet*))

;; ---------------------------------------------------------------------------------------------------

(define (tokenize expression)
  (define/match (tokenize* expression)
    [((? symbol?))
     (define identifier/string (~a expression))
     (define identifier/string/length (string-length identifier/string))
     (define identifier/multiple-characters? (> identifier/string/length 1))
     (define identifier/starts-with-@? (string-prefix? identifier/string "@"))
     (define separate-@-prefix? (and identifier/multiple-characters? identifier/starts-with-@?))
     (define identifier/index/start (if separate-@-prefix? 1 0))
     (define identifier/transformed
       (string->symbol (substring identifier/string identifier/index/start)))
     `(,@(if separate-@-prefix? '(@) '()) ,identifier/transformed)]
    [((? list?)) `(,(append-map tokenize* expression))]
    [(_) `(,expression)])

  (first (tokenize* expression)))

(module+ test
  (check-equal? (css-expr->css (css-expr [@media screen #:background-color white]))
                "@media screen{background-color:white;}"))

;; ---------------------------------------------------------------------------------------------------

(define-simple-macro (css-expr any ...) `(any ...))

(define (css-expr->css css-expr)
  (css/core->string
   (flatten/qualified-rules
    (normalize-nested-selectors
     (no-selector-parent-on-top-level!
      (flatten/at-rules
       (flatten/declarations
        (css/full->css/with-nested-declarations
         (parse
          (tokenize css-expr))))))))))
