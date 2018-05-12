#lang scribble/manual

@(require (for-label racket css-expr) racket/sandbox scribble/eval)

@title{CSS-expressions}
@author{@author+email["Leandro Facchinetti" "me@leafac.com"]}

@defmodule[css-expr]

@emph{S-expression-based CSS.}

@tabular[#:style 'boxed
         #:sep @hspace[1]
         #:row-properties '(bottom-border)
         `((, @bold{Version} , @seclink["changelog/0.0.2"]{0.0.2})
           (, @bold{Documentation} , @hyperlink["https://docs.racket-lang.org/css-expr/"]{Racket Documentation})
           (, @bold{License} , @hyperlink["https://gnu.org/licenses/gpl-3.0.txt"]{GNU General Public License Version 3})
           (, @bold{Code of Conduct} , @hyperlink["http://contributor-covenant.org/version/1/4/"]{Contributor Covenant v1.4.0})
           (, @bold{Distribution} , @hyperlink["https://pkgs.racket-lang.org/package/css-expr"]{Racket Package})
           (, @bold{Source} , @hyperlink["https://github.com/leafac/css-expr"]{GitHub})
           (, @bold{Bug Reports} , @hyperlink["https://github.com/leafac/css-expr/issues"]{GitHub Issues})
           (, @bold{Contributions} , @hyperlink["https://github.com/leafac/css-expr/pulls"]{GitHub Pull Requests}))]

@section[#:tag "overview"]{Overview}

S-expressions are a convenient way of representing hierarchical data. Racket’s syntax is based on S-expressions and it includes particular @emph{dialects} to support embedded domain-specific languages. For example, one can use X-expressions to represent XML in general or HTML in specific. Embedding documents in Racket programs as X-expressions has several benefits: parts of the document can be generated programmatically, the text editor can syntax-highlight and indent the code without extra effort and transformations on the document are data structure manipulations which do not require custom tools.

@margin-note{CSS-expressions go great with @hyperlink["https://docs.racket-lang.org/pollen"]{Pollen} and @hyperlink["https://docs.racket-lang.org/pollen-component"]{Pollen Component}.}

CSS is a domain-specific language for styling documents. Documents can be generated with X-expressions, so it is natural to want a S-expression-based representation for CSS as well. We introduce CSS-expressions, a domain-specific language embedded in Racket which is based on S-expressions and outputs CSS. In addition to the benefits of embedded domain-specific languages already mentioned, CSS-expressions also supports extended features generally found in CSS preprocessors including @hyperlink["http://sass-lang.com/"]{Sass}, @hyperlink["http://lesscss.org/"]{Less} and @hyperlink["http://stylus-lang.com/"]{Stylus}. For example, nested rules and declarations. Finally, because CSS-expressions are embedded in Racket, many features from the CSS preprocessors are @emph{free}: variables, mixins, operations, and so on.

@examples[
 (require racket/format css-expr)

 (~a
  (substring
   (css-expr->css
    (css-expr
     (code:comment "Styles from http://bettermotherfuckingwebsite.com/")
     [body
      #:margin (40px auto)
      #:max-width 650px
      #:line-height 1.6
      #:font-size 18px
      #:color |#444|
      #:padding (0 10px)]
     [h1 h2 h3
      #:line-height 1.2]))
   0 69) "...")]

@section[#:tag "installation"]{Installation}

CSS-expressions are a @hyperlink["https://pkgs.racket-lang.org/package/css-expr"]{Racket package}. Install it in DrRacket or with the following command line:

@nested[#:style 'code-inset
        @verbatim|{
$ raco pkg install css-expr
         }|]

@section[#:tag "usage"]{Usage}

@margin-note{See CSS-expressions in action on my @hyperlink["https://www.leafac.com"]{personal website} (@hyperlink["https://github.com/leafac/www.leafac.com/tree/pollen-component"]{source}). It also uses @hyperlink["https://docs.racket-lang.org/pollen-component"]{Pollen Component}.}

First, require the library. In Racket, add the following line:

@racketblock[(require css-expr)]

Or, in Typed Racket, add the following line:

@racketblock[(require css-expr/typed)]

Then, build CSS-expressions with @racket[css-expr]. Finally, use @racket[css-expr->css] to transform CSS-expressions into CSS.

@defform[(css-expr expr ...)]{
 Creates an CSS-expression from the given expressions. Escape to Racket code with unquoting (@racket[,expr]). @racket[css-expr] works like quasiquoting and only exists to clarify the intent of creating CSS-expressions.

 @examples[
 (require css-expr)

 (define main-width (css-expr 700px))
 (css-expr
  [body #:width ,@main-width]
  [.menu #:width ,@main-width])]
}

@defproc[(css-expr->css [css-expr Sexp])
         String]{
 Compiles a CSS-expression @tech[#:key "language/stylesheet"]{stylesheet} to CSS.
}

@section[#:tag "language"]{Language}

The following subsections describe CSS-expressions with English and examples. A @seclink["language/grammar"]{formal grammar} is also available in the next section.

@subsection[#:tag "language/stylesheet"]{Stylesheet}

A complete CSS-expression is, at the top level, a @deftech[#:key "language/stylesheet"]{stylesheet}, which is a list of @seclink["language/rule"]{rules}.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [body
    #:margin (40px auto)]
   [h1 h2 h3
    #:line-height 1.2]))]

In the example above, @racket[([body ...] [h1 h2 h3 ...])] is a stylesheet and each of @racket[[body ...]] and @racket[[h1 h2 h3 ...]] are rules.

@subsection[#:tag "language/rule"]{Rule}

There are two kinds of rules: @tech[#:key "language/rule/qualified-rule"]{qualified rules} and @tech[#:key "language/rule/at-rule"]{at-rules}. @deftech[#:key "language/rule/qualified-rule"]{Qualified rules} contain @seclink["language/declaration"]{declarations} for the styles in the document.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [body
    #:margin (40px auto)
    #:font-family "Fira Sans"]))]

In the example above, @racket[[body ...]] is a qualified rule and each of @racket[#:margin (40px auto)] and @racket[#:font-family "Fira Sans"] are declarations.

@deftech[#:key "language/rule/at-rule"]{At-rules} contain elements that control the stylesheet itself.

@margin-note{Identifiers starting with @racket[|@|] can be troublesome to generate programmatically (for example, @racket[(string->symbol (~a "@" some-computation))]). So @racket[[|@| name ...]] is an at-rule equivalent to @racket[[|@name| ...]].}

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [|@import| "other-stylesheet.css"]))
 (css-expr->css
  (css-expr
   [|@font-face| #:font-family "Fira Sans" #:src "..."]))
 (css-expr->css
  (css-expr
   [|@media| (and screen (#:min-width 700px))
    [body #:font-size 20px]]))]

In the example above, @racket[[|@import| ...]], @racket[[|@font-face| ...]] and @racket[[|@media| ...]] are at-rules. They differentiate from qualified rules by the @racket[|@|] prefix. The rule @racket[[|@import| ...]] is requesting the browser to load another stylesheet; it shows how at-rules handle @seclink["language/at-rule-expression"]{expressions} (in the example, the expression is @racket["other-stylesheet.css"]). The rule @racket[[|@font-face| ...]] is defining a new font and showing how at-rules can include declarations (in the example, @racket[#:font-family "Fira Sans"] and @racket[#:src "..."] are declarations). Finally, the rule @racket[[|@media| ...]] is declaring qualified rules that only apply under certain conditions; showing that at-rules can include qualified rules (in the example, @racket[[body ...]]) in addition to expressions (in the example, @racket[(and ...)]).

@subsection[#:tag "language/nesting"]{Nesting}

CSS-expressions allow for arbitrary nesting of @seclink["language/rule"]{rules} within one another, which is an extension to plain CSS generally found in preprocessors including @hyperlink["http://sass-lang.com/"]{Sass}, @hyperlink["http://lesscss.org/"]{Less} and @hyperlink["http://stylus-lang.com/"]{Stylus}. The nested rules are unnested during the process of translating CSS-expressions into CSS.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [.menu #:width 700px
    [.item #:text-decoration none]]))
 (css-expr->css
  (css-expr
   [.menu #:width 700px
    [|@media| (#:min-width 500px)
     #:color green]]))
 (css-expr->css
  (css-expr
   [|@media| screen
    [|@media| (#:min-width 700px)
     [body #:font-size 20px]]]))]

In the first example, nested qualified rules illustrate how to compactly define components. In the second example, a media query is nested within a qualified rule, which is convenient for responsive design. The third example shows how nested at-rules compose.

When nesting qualified rules, the default combinator for @seclink["language/selector"]{selectors} is the @emph{descendant} combinator, which in CSS is pronounced with a space—see example above. To combine selectors differently, it is necessary to explicitly refer to the selector of the parent qualified rule in the selector of the nested qualified rule. Accomplish that using @racket[&].

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [.menu #:width 700px
    [(> & .item) #:text-decoration none]]))]

In the example above, the declaration @racket[#:text-decoration none] is only effective on @racket[.item]s that are immediate children of @racket[.menu]s.

@margin-note{The following kinds of parent selectors allow for an added suffix with @racket[&-]: identifiers, namespaced selectors, prefixed selectors or combinations in which the last selector is one of the previous kinds.}

Under special conditions one can declare nested rules that add a suffix to the parent selector. Accomplish that using @racket[&-].

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [.menu #:width 700px
    [(&- item) #:text-decoration none]]))
 (css-expr->css
  (css-expr
   [|#main| #:color blue
    [(&- sidebar) #:color pink]]))]

@subsection[#:tag "language/at-rule-expression"]{At-Rule Expression}

At-rule expressions occur in @tech[#:key "language/rule/at-rule"]{at-rules}, after the @racket[|@name|] and before any @seclink["language/declaration"]{declarations} or inner @seclink["language/rule"]{rules}. In its simplest form, an at-rule expression is a @seclink["language/value"]{value}.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [|@import| "other-stylesheet.css"]))]

In the example above, @racket["other-stylesheet.css"] is a value standing for an at-rule expression.

Multiple at-rule expressions in a list translate as a composite expression in CSS.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [|@import| ("other-stylesheet.css" screen)]))]

In the example above, the list @racket[("other-stylesheet.css" screen)] translated to the composite expression @racket["other-stylesheet.css" screen] in CSS.

Multiple at-rule expressions in a row translate to alternative expressions in CSS.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [|@media| screen print
    [body #:line-height 1.2]]))]

In the example above, each of @racket[screen] and @racket[print] are at-rule expressions on their own. They translate to alternative expressions in CSS—separated by the comma.

@seclink["language/declaration"]{Declarations} can be at-rule expressions, and they have to be surrounded by parenthesis to be distinguished from declarations in the body of the at-rule.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [|@media| (#:min-width 700px)
    [body #:line-height 1.2]]))
 (css-expr->css
  (css-expr
   [|@font-face| #:font-family "Fira Sans" #:src "..."]))]

In the first example, @racket[(#:min-width 700px)] is a declaration working as an at-rule expression. Note the surrounding parenthesis; they are necessary to differentiate from declarations in the at-rule body, the case which the second example illustrates.

The last type of at-rule expressions is operations involving other at-rule expressions.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [|@media| (and screen (#:min-width 700px))
    [body #:line-height 1.2]]))
 (css-expr->css
  (css-expr
   [|@media| (or screen print)
    [body #:line-height 1.2]]))
 (css-expr->css
  (css-expr
   [|@media| (not (and screen (#:min-width 700px)))
    [body #:line-height 1.2]]))
 (css-expr->css
  (css-expr
   [|@media| (only screen)
    [body #:line-height 1.2]]))]

@subsection[#:tag "language/declaration"]{Declaration}

Declarations set CSS properties. In CSS-expressions, declarations are the property names as keywords followed by @seclink["language/value"]{values} and an optional @racket[!important] flag.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [body
    #:line-height 1.2
    #:background-color black]))
 (css-expr->css
  (css-expr
   [body
    #:line-height 1.2 !important]))]

In the first example above, @racket[#:line-height 1.2] and @racket[#:background-color black] are declarations. The properties names are @racket[#:line-height] and @racket[#:background-color], while @racket[1.2] and @racket[black] are values. The second example above illustrates the use of the @racket[!important] flag.

CSS-expressions also support an extension generally found in CSS preprocessors including @hyperlink["http://sass-lang.com/"]{Sass}, @hyperlink["http://lesscss.org/"]{Less} and @hyperlink["http://stylus-lang.com/"]{Stylus}: nested declarations.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [body
    #:font
    (#:size 18px
     #:family Helvetica)]))]

In the example above, the @racket[#:font] common prefix has been factored out from the declarations of @racket[#:font-size] and @racket[#:font-family].

Note that values can occur before the nested declarations.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [body
    #:font italic
    (#:size 18px
     #:family Helvetica)]))]

In the example above, @racket[italic] is a value before the nested declarations @racket[(#:size ...)].

@subsection[#:tag "language/value"]{Value}

This simplest kinds of value are symbols, numbers and strings.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [body
    #:font-style italic
    #:line-height 3.5
    #:font-family "Fira Sans"]))]

In the example above, @racket[italic] is a symbol value, @racket[3.5] is a number value and @racket["Fira Sans"] is a string value. Note how strings are quoted in the CSS output while symbols are not.

Similar to what happens in @seclink["language/at-rule-expression"]{at-rule expressions}, composite values must be enclosed in parenthesis.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [body
    #:font (italic 18px "Fira Sans")]))]

In the example above, @racket[(italic 18px "Fira Sans")] is a composite value.

A list of values is just laid out after the property name, without enclosing parenthesis.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [body
    #:font-family "Fira Sans" sans-serif]))]

In the example above, @racket["Fira Sans"] and @racket[sans-serif] compose a list of values.

@margin-note{Writing colors as @racket["#ff00dd"] does not work, because the CSS would include quotes around the value.}

Hexadecimal colors in CSS-expressions are just symbols, but it is necessary to escape the hash because of Racket’s parsing rules.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [body
    #:color |#ff00dd|]))]

In the example above, one can write the color as either @tt{|#ff00dd|} or @tt{\#ff00dd}.

@margin-note{Measurement symbols can be troublesome to generate programmatically (for example, @racket[(string->symbol (~a some-computation "px"))]). So @racket[(px 12)] is a value equivalent to @racket[12px].}

Measurements are also just symbols.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [body
    #:margin-left 12px]))]

In the example above, @racket[12px] is a measurement. Valid measurement units are: @racket[%], @racket[em], @racket[ex], @racket[ch], @racket[rem], @racket[vw], @racket[vh], @racket[vmin], @racket[vmax], @racket[cm], @racket[mm], @racket[q], @racket[in], @racket[pt], @racket[pc], @racket[px], @racket[deg], @racket[grad], @racket[rad], @racket[turn], @racket[s], @racket[ms], @racket[hz], @racket[khz], @racket[dpi], @racket[dpcm] and @racket[dppx].

@margin-note{Note that @tt{apply} is a form in CSS-expressions. It results in CSS that looks like function application, which is not equivalent to unquoting from the CSS-expression and writing a Racket expression using Racket’s @racket[apply]. For example, @racket[(apply calc 2px)] translates to @tt{calc(2px)}, while @racket[(px ,(apply + '(1 1)))] translates to @racket[2px]. Both are valid forms, useful in different contexts.}

Use @racket[apply] to write values that look like function application.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [body
    #:color (apply rgb 20 30 40)]))]

In the example above, @racket[(apply rgb 20 30 40)] is a value that looks like a function application in CSS.

@margin-note{Note that operations on values are forms in CSS-expressions. They result in operations in CSS, which are not equivalent to unquoting from the CSS-expression and writing Racket expressions. For example, @racket[(+ 2px 3px)] translates to @racket[2px + 3px], while @racket[(px ,(+ 2 3))] translates to @racket[5px]. Both are valid forms, useful in different contexts.}

Operations on values are also valid values.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [body
    #:width (apply calc (- 12px 2px))]))]

In the example above, @racket[(- 12px 2px)] is an operation. Valid operands are @racket[+], @racket[-], @racket[*] and @racket[/].

@subsection[#:tag "language/selector"]{Selector}

The simplest kind of selector is an identifier.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [li #:width 700px]))]

In the example above, @racket[li] is a selector that matches all occurrences of the @racket[<li>] HTML tag.

Multiple selectors in a row turn into a list.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [li a #:width 700px]))]

In the example above, the rule matches any @racket[<li>] and any @racket[<a>] HTML tag.

Enclose selectors in parenthesis to combine them with the descendant combinator—which in CSS is pronounced with a space.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [(li a) #:width 700px]))]

In the example above, the rule matches any link (@racket[<a>]) which occurs anywhere within a list item (@racket[<li>]).

@margin-note{Namespaced symbols can be troublesome to generate programmatically (for example, @racket[(string->symbol (~a some-computation "|" other-computation))]). So @racket[(\| ns a)] is a selector equivalent to @racket[ns\|a]. Again, note how it is necessary to escape the pipe (@tt{|}).}

Selectors can occur under a namespace.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [ns\|a #:width 700px]))]

In the example above, the rule only matches @racket[<a>] tags under the @racket[ns] namespace. Note that the pipe (@tt{|}) needs escaping because of Racket’s parsing rules for identifiers.

@margin-note{
 Prefixed symbols can be troublesome to generate programmatically (for example, @racket[(string->symbol (~a some-computation "#" other-computation))]). So @racket[(\# main)] is a selector equivalent to @racket[|#main|], and @racket[(\# div main)] is a selector equivalent to @racket[|div#main|]. Again, note how it is necessary to escape the hash (@tt{#}).

 This applies to all prefixed selectors. For example, @racket[(\. a menu)] is equivalent to @racket[a.menu]. Note that the dot alone (@tt{.}) has special meaning in Racket, so it also requires escaping.
}

Selectors may require prefixes.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [|#menu| #:width 700px]))
 (css-expr->css
  (css-expr
   [.menu-item #:width 700px]))
 (css-expr->css
  (css-expr
   [a:hover #:width 700px]))
 (css-expr->css
  (css-expr
   [a::before #:width 700px]))]

In the first example above, note that the hash—the prefix for selecting by id—needs escaping because of Racket’s parsing rules for identifiers. The alternative spelling @tt{\#menu} works the same.

@margin-note{Note that @tt{apply} is a form in CSS-expressions. It results in CSS that looks like function application, which is not equivalent to unquoting from the CSS-expression and writing a Racket expression using Racket’s @racket[apply]. For example, @racket[(apply nth-child 2)] translates to @tt{nth-child(2)}, while @racket[(\. ,(apply string-downcase '(ODD)))] translates to @racket[.odd]. Both are valid forms, useful in different contexts.}

Pseudo-classes that look like function applications use the @racket[apply] form.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [(: a (apply nth-child 2)) #:width 700px]))]

In the example above, @racket[(apply nth-child 2)] translates to a pseudo-class that looks like function application: @tt{nth-child(2)}.

The arguments to those pseudo-classes that look like function applications can be selectors, other nested pseudo-classes that look like function application and @racket[An+B] forms.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [(: a (apply not .classy)) #:width 700px]))
 (css-expr->css
  (css-expr
   [(: a (apply nth-child odd)) #:width 700px]))
 (css-expr->css
  (css-expr
   [(: a (apply nth-child even)) #:width 700px]))
 (css-expr->css
  (css-expr
   [(: a (apply nth-child 3)) #:width 700px]))
 (css-expr->css
  (css-expr
   [(: a (apply nth-child n)) #:width 700px]))
 (css-expr->css
  (css-expr
   [(: a (apply nth-child (n 2))) #:width 700px]))
 (css-expr->css
  (css-expr
   [(: a (apply nth-child (n 2 1))) #:width 700px]))
 (css-expr->css
  (css-expr
   [(: a (apply nth-child (n+ 2))) #:width 700px]))]

In the first example above, @racket[(apply not .classy)] is a pseudo-class that looks like function application whose argument another selector (@racket[.classy]). The rest of the examples illustrate the @racket[An+B] form.

@margin-note{See @secref["language/nesting"] for the specific cases in which it is valid to add a suffix to the parent selector with @racket[&-].}

In @seclink["language/nesting"]{nested qualified rules}, the selector @racket[&] stands for the parent selector. The selector @racket[&-] is a reference to the parent selector that allows for suffixes in limited cases.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [.menu #:width 700px
    [(> & .item) #:text-decoration none]]))
 (css-expr->css
  (css-expr
   [.menu #:width 700px
    [(&- item) #:text-decoration none]]))
 (css-expr->css
  (css-expr
   [|#main| #:color blue
    [(&- sidebar) #:color pink]]))]

@margin-note{Note that the @tt{||} combinator requires escaping because of Racket’s parsing rules.}

One can combine selectors in various ways.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [(+ .menu .item) #:text-decoration none]))
 (css-expr->css
  (css-expr
   [(> .menu .item) #:text-decoration none]))
 (css-expr->css
  (css-expr
   [(~ .menu .item) #:text-decoration none]))
 (css-expr->css
  (css-expr
   [((// for) .menu .item) #:text-decoration none]))
 (css-expr->css
  (css-expr
   [(\|\| .menu .item) #:text-decoration none]))]

Attribute-based selectors use the @racket[attribute] form.

@examples[
 (require css-expr)

 (css-expr->css
  (css-expr
   [(attribute .menu selected) #:text-decoration none]))
 (css-expr->css
  (css-expr
   [(attribute .menu (= href "...")) #:text-decoration none]))
 (css-expr->css
  (css-expr
   [(attribute .menu (~= href (case-insensitive "...")))
    #:text-decoration none]))]

Valid operands for attributes are: @racket[=], @racket[~=], @racket[^=], @racket[$=], @racket[*=] and @racket[\|=].

@section[#:tag "language/grammar"]{Grammar}

@racketgrammar*[
 #:literals (|@| not only and or !important? apply % em ex ch rem vw vh vmin vmax cm mm q in pt pc px deg grad rad turn s ms hz khz dpi dpcm dppx + - * / & &- \. \# : :: odd even n n+ = ~= ^= $= *= \|= case-insensitive + > ~ // \|\|)
 [stylesheet
  (rule ...)]
 [rule
  [selector ...+ block-element ...+]
  [|@| name at-rule/expression ... block-element ...]]
 [block-element
  declaration
  rule]
 [at-rule/expression
  value
  declaration
  (at-rule/expression ...+)
  (at-rule/expression/operation/operator/unary at-rule/expression)
  (at-rule/expression/operation/operator/n-ary at-rule/expression ...+)]
 [at-rule/expression/operation/operator/unary
  not only]
 [at-rule/expression/operation/operator/n-ary
  and or]
 [declaration
  (code:line #:name value ... !important? (declaration ...+)?)]
 [value
  identifier
  number
  string
  (value/measurement/unit number)
  (value/operation/operator value ...+)
  (apply name value ...+)
  (value ...+)]
 [value/measurement/unit
  % em ex ch rem vw vh vmin vmax cm mm q in pt pc px deg grad rad turn s ms hz khz dpi dpcm dppx]
 [value/operation/operator
  + - * /]
 [selector
  name
  (selector ...+)
  selector/namespaced
  (selector/prefixed/prefix selector? selector/prefixed/subject)
  (attribute selector? selector/attribute/subject)
  (selector/combination/combinator combinand:selector ...+)
  &
  (&- suffix)]
 [selector/namespaced
  (\| name? name)]
 [selector/prefixed/prefix
  \. \# : ::]
 [selector/prefixed/subject
  name
  selector/function/application]
 [selector/function/application
  (apply name selector/function/argument ...+)]
 [selector/function/argument
  selector
  selector/An+B
  selector/function/application]
 [selector/An+B
  odd even integer n
  (n integer integer?)
  (n+ integer)]
 [selector/attribute/subject
  name
  selector/namespaced
  (selector/attribute/operation/operator
   selector/attribute/operation/operand
   selector/attribute/operation/operand)]
 [selector/attribute/operation/operator
  = ~= ^= $= *= \|=]
 [selector/attribute/operation/operand
  name
  string
  (case-insensitive string)]
 [selector/combination/combinator
  + > ~ (// name) \|\|]]

@section[#:tag "acknowledgments"]{Acknowledgments}

Thank you @hyperlink["http://typographyforlawyers.com/about.html"]{Matthew Butterick} for Pollen—which motivated the creation of CSS-expressions—and for the feedback given in private email conversations. Thank you Greg Trzeciak for the @hyperlink["https://groups.google.com/forum/#!topic/racket-users/uZs169VKXBE"]{early feedback}. Thank you all Racket developers. Thank you all users of this library.

@section[#:tag "changelog"]{Changelog}

This section documents all notable changes to CSS-expressions. It follows recommendations from @hyperlink["http://keepachangelog.com/"]{Keep a CHANGELOG} and uses @hyperlink["http://semver.org/"]{Semantic Versioning}. Each released version is a Git tag.

@;{
 @subsection[#:tag "changelog/unreleased"]{Unreleased} @; 0.0.1 · 2016-02-23

 @subsubsection[#:tag "changelog/unreleased/added"]{Added}

 @subsubsection[#:tag "changelog/unreleased/changed"]{Changed}

 @subsubsection[#:tag "changelog/unreleased/deprecated"]{Deprecated}

 @subsubsection[#:tag "changelog/unreleased/removed"]{Removed}

 @subsubsection[#:tag "changelog/unreleased/fixed"]{Fixed}

 @subsubsection[#:tag "changelog/unreleased/security"]{Security}
}

@subsection[#:tag "changelog/0.0.2"]{0.0.2 · 2017-03-08}

@subsubsection[#:tag "changelog/0.0.2/changed"]{Changed}

@itemlist[
 @item{Re-implemed CSS-expression from scratch using @hyperlink["https://docs.racket-lang.org/nanopass/index.html"]{Nanopass}. The user interface is the same.}]

@subsection[#:tag "changelog/0.0.1"]{0.0.1 · 2017-02-01}

@subsubsection[#:tag "changelog/0.0.1/added"]{Added}

@itemlist[
 @item{Basic functionality.}]

@section{References}

@itemlist[
 #:style 'ordered
 @item{@hyperlink["https://www.w3.org/Style/CSS/read.en.html"]{Understanding the CSS Specifications}.}
 @item{@hyperlink["https://www.w3.org/TR/CSS/"]{CSS Snapshot 2015}.}
 @item{@hyperlink["https://www.w3.org/Style/CSS/specs.en.html"]{CSS spec­i­fi­ca­tions}.}
 @item{@hyperlink["https://www.w3.org/TR/css-syntax-3/"]{CSS Syntax Module Level 3}.}
 @item{@hyperlink["https://drafts.csswg.org/css-syntax/"]{CSS Syntax Module Level 3 Editor's Draft}.}
 @item{@hyperlink["https://www.w3.org/TR/CSS2/"]{Cascading Style Sheets Level 2 Revision 1 (CSS 2.1) Specification}.}
 @item{@hyperlink["https://www.w3.org/TR/CSS22/"]{Cascading Style Sheets Level 2 Revision 2 (CSS 2.2) Specification}.}
 @item{@hyperlink["https://www.w3.org/TR/css3-selectors/"]{Selectors Level 3}.}
 @item{@hyperlink["https://www.w3.org/TR/selectors4/"]{Selectors Level 4}.}
 @item{@hyperlink["https://www.w3.org/TR/css3-values/"]{CSS Values and Units Module Level 3}.}
 @item{@hyperlink["https://www.w3.org/TR/css3-mediaqueries/"]{Media Queries}.}
 @item{@hyperlink["https://www.w3.org/TR/mediaqueries-4/"]{Media Queries Level 4}.}
 @item{@hyperlink["https://www.w3.org/TR/css3-color/"]{CSS Color Module Level 3}.}
 @item{@hyperlink["https://www.w3.org/TR/css-color-4/"]{CSS Color Module Level 4}.}
 @item{@hyperlink["https://www.w3.org/TR/css-fonts-3/"]{CSS Fonts Module Level 3}.}
 @item{@hyperlink["http://sass-lang.com/documentation/file.SASS_REFERENCE.html"]{Sass (Syntactically Awesome StyleSheets)}.}
 @item{@hyperlink["http://lesscss.org/features/"]{Less}.}
 @item{@hyperlink["http://stylus-lang.com/"]{Stylus}.}]
