#lang typed/racket/base
(require typed/rackunit "../typed.rkt")

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

;; value/function/application
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

;; selector/function/application
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
(check-equal? (css-expr->css (css-expr [(attribute (= href (case-insensitive "https://example.com")))
                                        #:background-color black]))
              "[href=\"https://example.com\" i]{background-color:black;}")

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
              (string-append "body{margin:40px auto;max-width:650px;line-height:1.6;font-size:18px;"
                             "color:#444;padding:0 10px;}h1,h2,h3{line-height:1.2;}"))

;; prepended `@'
(check-equal? (css-expr->css (css-expr [body #:background-color black
                                             [@media screen #:background-color white]]))
              "body{background-color:black;}@media screen{body{background-color:white;}}")

;; nested qualified-rule
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
(check-equal? (css-expr->css (css-expr [(\# main) [(&- sidebar) #:background-color |#ffffff|]]))
              "#main-sidebar{background-color:#ffffff;}")
(check-equal? (css-expr->css (css-expr [p [span #:background-color pink]
                                          [a #:background-color black]]))
              "p span{background-color:pink;}p a{background-color:black;}")
;; The following reordering of declarations is weird to me, but is how Sass behaves.
(check-equal? (css-expr->css (css-expr [body
                                        #:margin 0
                                        [p #:background-color |#ffffff|]
                                        #:padding 0]))
              "body{margin:0;padding:0;}body p{background-color:#ffffff;}")

;; nested at-rules
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
              (string-append "body{background-color:black;}"
                             "@media screen{body{background-color:white;}}"
                             "@import \"styles.css\";"))
(check-equal? (css-expr->css (css-expr [body #:background-color black
                                             [@ media screen #:background-color black]
                                             [@ media print #:background-color white]]))
              (string-append "body{background-color:black;}"
                             "@media screen{body{background-color:black;}}"
                             "@media print{body{background-color:white;}}"))

;; nested declarations
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
              "@media (max-width:700px) and (max-height:600px){p{background-color:black;}}")
