#lang scribble/manual
@(require scribble/eval scribble/core (for-label "fmt.rkt" racket) racket)

@title{A simple formatter for Racket}
@author{Jacob J. A. Koot}
@(defmodule format/fmt #:packages ())
@;@(defmodule "fmt.rkt" #:packages ())

@(define-syntax (deffmt stx)
   (syntax-case stx ()
     ((_ #:tag tag x y ...)
      #'(begin
          (elemtag tag)
          (defidform #:kind "fmt instruction" #:link-target? #f x y ...)))
     ((_ x y ...)
      #'(begin
          (elemtag x)
          (defidform #:kind "fmt instruction" #:link-target? #f x y ...)))))

@(define-syntax-rule (Interaction x ...)
   (interaction #:eval (make-base-eval #:lang '(begin (require racket "fmt.rkt")
                                                 (print-as-expression #f))) x ...))

@(define-syntax-rule (Interaction/no-prompt x ...)
   (interaction/no-prompt #:eval (make-base-eval #:lang '(begin (require racket "fmt.rkt")
                                                           (print-as-expression #f))) x ...))

@(define (inset . x) (apply nested #:style 'inset x))
@(define (note . x) (inset (apply smaller x)))

@(define-syntax (example stx)
   (syntax-case stx ()
     ((_ a)
      #'(begin @racket[a] @literal{ → } @element['tt (format "~a" a)] @(linebreak)))))

@(define (red str)
   (define prop:red (color-property "red"))
   (define red (style #f (list prop:red)))
   (element red str))

@(define (green str)
   (define prop:red (color-property "green"))
   (define green (style #f (list prop:red)))
   (element green str))

@(define(minus) (element 'tt "-"))

@(define-syntax-rule (ignore x ...) (void))

@section{Rationale}

Simple formatting tools can be useful when readability for the human eye is of some
importance, but not to the extent that a highly finished presentation is required.
@nonbreaking{An example} of a simple formatter of
@(hyperlink #:underline? #t "http://racket-lang.org/" "Racket")
is procedure @racket[format].
It is a handy tool, @nonbreaking{but in some} cases may not provide enough functionality.
Module
@seclink["format" #:doc '(lib "scribblings/reference/reference.scrbl")]{racket/format}
provides elaborated padding and several numerical formats,
but is somewhat verbose when the details are to be specified.
Which functions and shape a simple formatter should have
is a matter of personal taste, I think.
In this document I show a simple formatter that
combines some of the powerful features of the format-statement of
@(hyperlink #:underline? #t "http://en.wikipedia.org/wiki/Fortran" "Fortran")
with the flexibility of Racket’s generic procedures @racket[write] and @racket[display].
The principles are simple enough to get started almost immediately.
Nevertheless tools for @seclink["Padding" "padding"],
@seclink["Tabulation" "tabulation"],
several @seclink["numerical" "numerical formats"],
@seclink["literal" "literals"],
@seclink["iteration" "iteration"] and
@seclink["compound" "compound"] and
@seclink["condition" "conditional"] instructions are included.
Field widths, tabulator positions and repetition counts can be constants within the format,
but can also be obtained from the data.
@red{Warning}: @seclink["Padding" "padding"]
and @seclink["Tabulation" "tabulation"] are implemented by insertion of spaces and
therefore require a fixed width character font,
such as @(hyperlink #:underline? #t "http://en.wikipedia.org/wiki/Courier_(typeface)" "Courier"),
in order to function properly.

@section{Format-procedures}

Formats can be regarded as procedures that transform data from one representation to another one.
Procedure @racket[fmt] is based on this idea.
It translates strings of @seclink["fmt-instructions"]{format-instructions} to a format-procedure,
that is, a Racket procedure that accepts data and returns the formatted external representation
as a string or sends it through an output-port.
If during formatting an error is detected, an exception is raised and no output is produced at all.
@red{Warning}: the amount of output that can be sent through an output port by one single
format-procedure call may be subjected to a platform dependent limit,
because the produced output is gathered in a string before being committed to the port.

@defproc[(fmt
           (format (or/c string? fmt?)) ...
           (port (or/c output-port? 'string 'str 'current 'cur 'argument 'arg) 'string))
         (and/c procedure? fmt?)]{
 Procedure @racket[fmt] returns a procedure, in particular a format-procedure,
 satisfying predicate @racket[fmt?] which implies satisfying predicate @racket[procedure?].

 The optional @racket[port]-argument can be placed at arbitrary position before,
 among or after the @racket[format]-arguments.
 The symbols @racket['string], @racket['current] and @racket['argument] can be abbreviated as
 @racket['str], @racket['cur] and @racket['arg].
 A @racket[format]-argument is either a @seclink["Format-strings" "format-string"] or
 a format-procedure made by procedure @racket[fmt].
 
 All format-strings are parsed,
 checked for correctness and translated to subprocedures of the format-procedure to be produced.
 If no @racket[format]-argument is given, @racket[""] is assumed.
 Adjacent format-strings are parsed as though concatenated to form one single string
 with commas inserted between the individual strings.
 The commas are separators.
 @seclink["avoid" "Repeated parsing and translation"] of identical format-strings
 is avoided.
 
 Each @racket[format]-argument consisting of a format-procedure is inserted
 as a compound instruction without being parsed or translated again.
 The @racket[port]s of the inserted format-procedures are ignored.}

Format-procedures are called as follows:

@defproc[#:kind "" #:link-target? #f
         ((fmt
            (format (or/c string? fmt?)) ...
            (port (or/c output-port? 'string 'str 'current 'cur) 'string))
          (datum any/c) ...)
         (or/c void? string?)]
@defproc[#:kind "" #:link-target? #f
         ((fmt
            (format (or/c string? fmt?)) ...
            (port (or/c 'argument 'arg)))
          (port-arg (or/c output-port? 'string 'str 'current 'cur))
          (datum any/c) ...)
         (or/c void? string?)]{

 When @racket[fmt] is called with @racket[port]-argument
 @racket['argument] or @racket['arg]
 the produced format-procedure requires the data to be preceded by a @racket[port-arg]ument.
 
 In both forms the format-procedure formats the data according to the
 @racket[format]-arguments from which it was built and
 the formatted data are sent through an output-port or returned as a string as indicated
 by argument @racket[port] cq @racket[port-arg].
 If the results are to be committed to an output-port,
 this port is checked to be open before starting formatting.
 
 @itemlist[
 (list
   @item{If the argument @racket[port] cq @racket[port-arg] is
   @racket['current] or @racket['cur],
   the formatted data are committed to the current output-port
   and the format-procedure returns void.
   For this purpose parameter @racket[current-output-port] is consulted at the time the
   format-procedure is called,
   not at the time the format-procedure is made by procedure @racket[fmt].}
   @item{If the argument @racket[port] cq @racket[port-arg] is
   an output-port, the formatted data are committed to this port
   and the format-procedure returns void.
   A format-procedure made with
   @nonbreaking{(@racket[fmt] (@racket[current-output-port]) @racket[format] ...)}
   commits the formatted data to the @racket[current-output-port] as found in this parameter
   at the time the format-procedure was made.}
   @item{If the argument @racket[port] cq @racket[port-arg] is
   @racket['string] or @racket['str],
   the formatted data are returned as a string.})]}

Examples:
@margin-note{@element["sroman"]{@smaller{In these examples instruction @elemref["I" "I5"]
   is given exact integer numbers
   and displays them right justified in fields of 5 characters.@(linebreak)
   In the results `◦´ is used to show spaces.@(linebreak)
   In the results proper they are spaces, of course.}}}
@racket[((fmt "I5") 12)] → @code{"◦◦◦12"}@(linebreak)
@racket[((fmt "I5" 'string) 12)] → @code{"◦◦◦12"}@(linebreak)
@racket[((fmt "I5" 'current) 12)] → void, displays @code["◦◦◦12"]@(linebreak)
@racket[((fmt "I5" (current-output-port)) 12)] → void, displays @code["◦◦◦12"]@(linebreak)
@racket[((fmt "I5" 'argument) 'string 12)] → @code{"◦◦◦12"}@(linebreak)
@racket[((fmt "I5" 'argument) 'current 12)] → void, displays @code["◦◦◦12"]@(linebreak)
@racket[((fmt "I5" 'argument) (current-output-port) 12)] → void, displays @code["◦◦◦12"]

Forgetting the @italic{@racket[port-arg]}ument when calling a format-procedure made with
@italic{@racket[port]}-argument @racket['argument] or @racket['arg] raises an exception
or may produce unexpected results:

@Interaction/no-prompt[ (define my-fmt (fmt "I5" 'argument))
                       (my-fmt 12)
                       ((fmt 'arg "D") (open-output-string))]

@defproc[#:kind "predicate" (fmt? (object any/c)) boolean?]{
 Returns @racket[#t] if the argument is a format-procedure made by procedure @racket[fmt],
 else @racket[#f].}

@defproc[#:kind "procedure" (fmt-port (format-procedure fmt?))
         (or/c output-port? 'current 'string 'argument)]{
 Returns the @italic{@code{port}}-argument with which the @racket[format-procedure]
 was made by means of procedure @racket[fmt].
 Returns symbol @racket['string] if @racket[fmt] was called without @italic{@code{port}}-argument.
 Abbreviations @racket['str], @racket['cur] and @racket['arg] are reported in their full forms,
 id est, @racket['string], @racket['current] and @racket['argument]. Examples:}

@code{(fmt-port (fmt 'cur))}  →  @code{current}@(linebreak)
@code{(fmt-port (fmt (open-output-string)))} → @racketfont{#<output-port:string>}@(linebreak)
@code{(fmt-port (fmt (current-output-port)))} → @racketfont{#<output-port>}

@section[#:tag "avoid"]{Repeated parsing and translation avoided}

Most examples in this document have the form:

@code{((fmt fmt-arg ...) datum ...)}

Procedure @racket[fmt] does not repeat parsing and translation
when called repeatedly with the same arguments.
For this purpose it uses two hashes with the arguments as keys and
the produced format-procedures or subprocedures as values.

@inset{@code{(define port (open-output-nowhere))}@linebreak[]
 @code{(define my-fmt (fmt port "f17.15/"))}@linebreak[]
 @code{(for ((i (in-range #e1e6))) (my-fmt (random)))}}

is not much faster than:

@inset{@code{(define port (open-output-nowhere))}@linebreak[]
 @code{(for ((i (in-range #e1e6))) ((fmt port "f17.15/") (random)))}}

In the last example, @racket["f17.15/"] is parsed and translated once only.

@ignore{@racket[fmt] can be implemented more efficiently by replacing it by a
 syntax that does the parsing and translation once when a program is compiled. In this form it
 would not parse and translate the format-strings each time a program is run.
 However, this would restrict @racket[fmt] to information available at expansion time only.
 As a procedure @racket[fmt] can handle format-strings and allows inclusion of format-procedures
 that are computed at run-time.
 Given procedure @racket[fmt] it is possible to construct a syntactic form,
 but with much less flexibility. The benefits would be small,
 for parsing and translation take only a small fraction of the time of the formatting proper,
 even when no hashes would be used.}

@defproc[(fmt-clear-hash) void?]{
 Empties the hash tables used by @racket[fmt].
 For example, when defining all format-procedures in a separate module,
 you may want to clear the memory occupied by the hash-tables at the end of this module.
 The garbage collector usually cannot remove them.}

@defproc[(fmt-hash-size) exact-nonnegative-integer?]{Returns the number of keys in the two hashes
 used by @racket[fmt].}

@section{Format-strings}

A format-string describes a format-procedure in its own simple language.
The format-instructions are non verbose.
Each elementary instruction consists of one single character,
possibly preceded by a repetition count and followed by one or more arguments,
such as a field width or tabulator position.
A format-string can include literal data, for example for headers.
There is no distinction between lower case and capital letters, except within literal data.
Separators, id est white space and commas, are irrelevant except in the following cases:

@itemlist[
 (list
   @item{Within a @seclink["literal" "literal"], separators are part of the literal.}
  
   @item{No separator must appear within a @seclink["arguments" "numerical argument"]
  or @seclink["arguments" "iteration count"].}
  
   @item{Some instructions may be followed by one or more
  @seclink["arguments" "numerical arguments"].
  These arguments may be omitted starting from the last one.
  However, when omitting one or more numerical arguments where the next instruction
  starts with a token that can be interpreted as a numerical argument, such as a repetition count,
  a comma is required as separator, optionally preceded and/or followed by other separators.
  Hence, no comma must appear before any numerical argument belonging to the preceding instruction.
  White space is required between two numerical arguments if the first character of the second one
  could be parsed as belonging to the previous one.
  Commas do no harm when the argument or arguments of the instruction are format-instructions,
  because these never are optional.}
  
   @item{The start and end of a @seclink["literal" "literal"]
  are marked by a single quote, but within a literal two
  immediately adjacent single quotes are read as one single quote belonging to the literal.
  Hence, where a literal is followed by another literal, a separator is required.}
  
   @item{Character @code{#\null} is not white space.
  It must not appear outside @seclink["literal" "literals"].@(linebreak)
  Advice: never use @code{#\null} or its equivalents such as @code{#\u0} in a format string.})]

@section[#:tag "fmt-instructions"]{Format-instructions}

@margin-note{@element["sroman"]{@smaller{
   See the @seclink["synopsis" "synopsis"] for a list of all format-instructions.}}}

A format-procedure is given data to be formatted.
A format-instruction that consumes a datum,
removes it from the list of data in order that
the next data consuming instruction gets the next datum.
An exception is raised if an attempt is made to execute
a data consuming instruction after all data already have been consumed or
when unconsumed data remain after completion of the format-procedure.
There are instructions that add data to the list of remaining data, possibly after consuming data.
The added data always are the first to be consumed next.
For example, instruction @elemref["%" "%"] consumes a datum, which must be a rational number,
and adds the numerator and denominator
as two separate exact integer numbers in front of the remaining data,
the numerator becoming the first datum, the denominator the second one.
Subsequent format-instructions can treat the numerator and denominator separately.
Example:

@code{((fmt "%I':'I") (/ 30 -40))} → @code{"-3:4"}

In this example instruction @elemref["%"]{%} consumes the datum @racket[30/-40] and adds
the numerator @racket[-3] and denominator @racket[4] to the list of remaining data,
which in this example is empty.@(linebreak)
The first @nonbreaking{@elemref["I"]{I-instruction}} displays the numerator.
The @elemref["simple-literal"]{literal} @code{"':'"} displays the colon.@(linebreak)
The second @nonbreaking{@elemref["I"]{I-instruction}} displays the denominator.
The output is collected in a string. @nonbreaking{This string is returned.}

When you do want the format procedure to return normally when given more data than consumed,
add @tt{"}@elemref["*ξ" "*"]@elemref["S" "S"]@tt{"} as the last format-instruction.
The unconsumed data will be ignored. Example:

@racket[((fmt "d*s") 1 2 3 4)] → @racket["1"]. 

When you do want the format-procedure to return normally when given less data than expected,
insert instruction @tt{"}@elemref["?" "?"]@elemref[";" ";"]@tt{"} where appropriate.
For example:

@racket[((fmt "5(?;d!x)") 1 2 3)] → @racket["1 2 3"]

In the above example @racket{5} indicates that in principle 5 data are expected.
However, @racket{?;} makes sure that the format-procedure does generate
the output and exits normally when less than @racket[5] data are found.

@subsection[#:tag "arguments"]{Instruction arguments}

In the description of the instructions, ξ represents an argument consisting of an instruction.
ν, μ and ε represent numerical arguments such as a field width or a tabulator position.
A repetition count has the same form as a numerical argument.
Omitted numerical arguments are zero. An omitted repetition count is one.
They can have one of the following three forms:

@tabular[
 #:sep @hspace[3] #:style 'top #:column-properties '(top)
 (list
   (list
     @verbatim["δ...+"]
     "A sequence of one or more decimal figures.")
  
   (list "" "")
  
   (list
     @verbatim["δ...‹period›"]
     "A sequence of zero or more decimal figures immediately followed by a period.
A period without immediately preceding decimal figure is\ninterpreted as zero.")
  
   (list "" "")
  
   (list
     @verbatim["#"]
     @list[
 "Consumes a datum, which must be a natural number ("
 @nonbreaking{@racket[exact-nonnegative-integer?]}
 "). This number is used as numerical argument or repetition count."]))]

@(define space-note
   @margin-note{@element["sroman"]{@smaller{`◦´ is used to show spaces.
   In the results proper they are spaces, of course.}}})

Examples: @space-note

@code{((fmt "I5") 12)} → @code{"◦◦◦12"} integer format, field width 5.@(linebreak)
@code{((fmt "I5.3") 12)} → @code{"◦◦012"} integer format, field width 5, at least 3 decimal figures.
@(linebreak)
@code{((fmt "I##") 5 3 12)} → @code{"◦◦012"} idem taking the arguments from the data.@(linebreak)
@code{((fmt "#'x'") 5)} → @code{"xxxxx"} taking the repetition count from the data.

@subsection{Elementary format-instructions}

@margin-note{@element["sroman"]{@smaller{See the Racket documentation:
   @hyperlink["http://docs.racket-lang.org/reference/printing.html"]{The Printer}
   for the distinction between @racket[display], @racket[write] and @racket[print].}}}

@deffmt["D"]{

 Consumes one datum and @racket[display]s it according to the current
 @seclink["Padding" "padding"] mode.
 If the datum is a string and padding is switched on,
 heading and trailing spaces are removed before the string is padded.
 No spaces are removed from or added to strings that are non trivial parts of the datum,
 for example in case the datum is a list of strings.
 In such a case you may want to unfold the list.
 This allows each element to be formatted individually. See section @secref["unfolding"].}

@deffmt["W"]{

 Consumes one datum and @racket[write]s it according to the current
 @seclink["Padding" "padding"] mode.}

@deffmt["P"]{

 Consumes one datum and @racket[print]s it according to the current
 @seclink["Padding" "padding"] mode.}

@deffmt["X"]{

 Displays one space.}

@deffmt["/"]{

 Executes a newline instruction and marks the start of the new line.}
See @seclink["Tabulation" "tabulation"].

@deffmt["|"]{

 Executes a newline instruction only if not at the start of the current line.}

Examples: @space-note
@code{((fmt 'current "DXDXD")   "Jacob" 3 #\x)} → void, displays @code{Jacob◦3◦x}@(linebreak)
@code{((fmt 'current "WXWXW")   "Jacob" 3 #\x)} → void, displays @code{"Jacob"◦3◦#\x}@(linebreak)
@code{((fmt 'current "D") (list "Jacob" 3 #\x))} → void, displays @code{(Jacob◦3◦x)}@(linebreak)
@code{((fmt 'current "W") (list "Jacob" 3 #\x))} → void, displays @code{("Jacob"◦3◦#\x)}

@subsection[#:tag "Padding"]{Padding}

Padding applies to @seclink["literal" "literal data"] and
the instructions
@elemref["D" "D"],
@elemref["W" "W"],
@elemref["P" "P"],
@elemref["B" "B"],
@elemref["O" "O"],
@elemref["H" "H"] and
@elemref["=" "="].
When padding is switched on,
these instructions add heading and/or trailing spaces
if otherwise less characters would be generated than indicated by the field width.
Instruction @elemref["D" "D"] first removes heading and trailing spaces when padding a string.
The same holds when padding a literal datum.
Output that does not fit within the field width is not truncated.
Initially padding is switched off.
When a format-procedure is called from another format-procedure,
the former inherits the padding mode from the latter.
If the called procedure alters the padding mode,
the alteration remains effective after return.
Instruction @elemref["A" "A"] can be used to restore the previous padding mode.
@red{Warning}: because padding is done by insertion of spaces,
it is useful only with a font of fixed character width.

@deffmt["N"]{

 Switches padding off. No spaces are removed or added.}

@deffmt[#:tag "L" "Lν"]{

 For left alignment in fields of ν characters.}

@deffmt[#:tag "R" "Rν"]{

 For right alignment in fields of ν characters.}

@deffmt[#:tag "C" "Cν"]{

 For centred alignment in fields of ν characters.
 When needed, spaces are added to the left and to the right in order to match the field width.
 If the number of spaces to be added is even, say @italic{2k},
 then @italic{k} spaces are added both at the left and at the right.
 If the number of spaces to be added is odd, say @italic{2k+1},
 then @italic{k+1} spaces are added to the left and @italic{k} spaces to the right.}

@deffmt[#:tag "A" "Aξ"]{

 Memorizes the current padding mode and field width,
 executes instruction ξ and upon completion restores the memorized padding mode and field width.

 Examples: @space-note
 @code{((fmt "L5*D") 1 2 3)} → @code{"1◦◦◦◦2◦◦◦◦3◦◦◦◦"}@(linebreak)
 @code{((fmt "R5*D") 1 2 3)} → @code{"◦◦◦◦1◦◦◦◦2◦◦◦◦3"}@(linebreak)
 @code{((fmt "C5*D") 1 2 3)} → @code{"◦◦1◦◦◦◦2◦◦◦◦3◦◦"}@(linebreak)
 @code{((fmt "N  D") "◦◦Jacob◦◦")} → @code{"◦◦Jacob◦◦"} ; no spaces removed, nor added.@(linebreak)
 @code{((fmt "L0 D") "◦◦Jacob◦◦")} → @code{"Jacob"} ; spaces removed, no spaces added.@(linebreak)
 @code{((fmt "L8 D") "◦◦Jacob◦◦")} → @code{"Jacob◦◦◦"} ; spaces first removed, then added.
}
@subsection[#:tag "literal" "Literal data"]

@deffmt[#:tag "simple-literal" "'κ ...'"]{

 Each κ is an arbitrary character,
 except that a single quote must be written as two immediately adjacent single quotes.
 Backslashes and double quotes must be escaped by backslashes as usual in a string,
 id est, as @literal{\\} or @literal{\"}.
 The string @literal{"κ ..."} is displayed according to the current padding mode.
 In literals the backslash can also be used for other escapes, for example:

 @Interaction[
 ((fmt "'a\nb'" 'cur))]}

@deffmt[#:tag "compound-literal" "^'κ ...'"]{ 

 @literal["'κ ...'"] has the same form as above.
 The string must contain zero or more symbolic expressions.
 They are not evaluated.
 They are read at parsing time and added to the list of remaining data during
 execution of the format-procedure.
 The leftmost symbolic expression becomes the first next datum.
 In the symbolic expressions each single quote must be written as
 two immediately adjacent single quotes.}

Because within a literal,
two adjacent single quotes are read as one single quote being part of the literal,
a separator is required between two adjacent literals when the second one is of the
@elemref["simple-literal" "first form"].

Examples: @space-note
@code{((fmt "R10'Article','Price'"))} → @code{"◦◦◦Article◦◦◦◦◦Price"}@(linebreak)
@code{((fmt "^'Article Price'R10 2D"))} → @code{"◦◦◦Article◦◦◦◦◦Price"}@(linebreak)
@code{((fmt "L2 ''''"))} → @code{"◦'"}@(linebreak)
@code{((fmt "L2 '' ''"))} → @code{"◦◦◦◦"}@(linebreak)
@code{((fmt 'cur "'\\'"))} → void, displays \@(linebreak)
@code{((fmt 'cur "'\"\\\"'"))} → void, displays @element['tt "\"\\\""]

@red{Warning}: in the arguments of procedure @racket[fmt],
@literal{'κ ...'} must not be split over two or more format-strings.
For example, the following raises an exception:

@Interaction/no-prompt[
 (fmt "'Article" "Price'")]

@subsection[#:tag "numerical"]{Numerical formats}

The numerical format-instructions @code{I}, @code{F} and @code{E} have their own padding,
independent from the padding mode described @seclink["Padding" "elsewhere"] in this document.

@deffmt[#:tag "I" "Iνμ"]{

 Instruction @code{I} is particularly useful for integer numbers,
 but nevertheless accepts every real number.
 It consumes and displays a real number rounded to an integer number.
 First the sign of the datum is determined.
 Subsequently the datum is rounded to an exact integer number.
 If rounding produces zero, the sign is retained.
 If a sign is to be written, it is placed immediately in front of the first decimal figure.
 Leading spaces are added if otherwise less than ν characters would be produced.
 In all cases the result contains at least μ decimal figures,
 padding with heading zeros when necessary,
 or at least one decimal figure if μ is zero.
 The exceptional numbers @code{±inf.0} and @code{±nan.0} are treated specially.
 They are right justified in a field of at least ν characters. Examples: 

 @space-note
 @code{((fmt "*I3") 2 3.4 5.6)} → @code{"◦◦2◦◦3◦◦6"}@(linebreak)
 @code{((fmt "*I3.2") 2 3.4 5.6)} → @code{"◦02◦03◦06"}@(linebreak)
 @code{((fmt "I") 0.0)} → @code{"0"}@(linebreak)
 @code{((fmt "I") -0.0)} → @code{"-0"}@(linebreak)
 @code{((fmt "I") 1.0e-100000)} → @code{"0"}@(linebreak)
 @code{((fmt "I") -1.0e-100000)} → @code{"-0"}@(linebreak)
 @code{((fmt "I")  1.0e1000000)} → @code{"+inf.0"}@(linebreak)
 @code{((fmt "I") -1.0e1000000)} → @code{"-inf.0"}@(linebreak)
 @code{((fmt "I10") (/ 0.0 0.0))} → @code{"◦◦◦◦+nan.0"}@(linebreak)
 @code{(string-length ((fmt "I") #e1e100000))} → @code{100001}}

@deffmt[#:tag "F" "Fνμ"]{

 Consumes and displays a real number in decimal expansion:
 leading spaces, [sign], integer part, period, fraction of exactly μ decimal figures.
 The datum is rounded such as to fit the width of the fraction.
 If rounding yields zero, the sign is retained.
 The result has at least one decimal figure before the period.
 Leading spaces are added if otherwise less than ν characters would be produced.
 The exceptional numbers @code{±inf.0} and @code{±nan.0} are treated specially.
 They are right justified in a field of at least ν characters. Examples: 

 @space-note

 @code{((fmt "*F3  ") 2 3.4 5.6)} → @code{"◦2.◦3.◦6."}@(linebreak)
 @code{((fmt "*F5.2") 2 3.4 5.6)} → @code{"◦2.00◦3.40◦5.60"}@(linebreak)
 @code{((fmt "F.4") 2/3)} → @code{"0.6667"}@(linebreak)
 @code{((fmt "D") 2/3)} → @code{"2/3"}@(linebreak)
 @code{((fmt "F")  1.0e1000000)} → @code{"+inf.0"}@(linebreak)
 @code{((fmt "F") -1.0e1000000)} → @code{"-inf.0"}@(linebreak)
 @code{((fmt "F.2") -1.0e-100000)} → @code{"-0.00"}@(linebreak)
 @code{((fmt "F") (/ 0.0 0.0))} → @code{"+nan.0"}}

@deffmt[#:tag "E" "Eνμε"]{

 Consumes and displays a real number in scientific notation:
 leading blanks, [sign],
 one decimal figure,
 period,
 exactly μ decimal figures,
 letter @tt{e}, sign of exponent,
 ε or more decimal figures of exponent.
 If the number is not zero, the exponent is chosen such that there is exactly one
 non-zero decimal figure before the decimal point.
 If the number is zero, all decimal figures are zero.
 Leading spaces are added if otherwise less than ν characters would be produced.
 The exceptional numbers @code{±inf.0} and @code{±nan.0} are treated specially.
 They are right justified in a field of at least ν characters. Examples:

 @space-note
 @code{((fmt "*E10.3.2") 2/3 2.3e-2)} → @code{"◦6.667e-01◦2.300e-02"}@(linebreak)
 @code{((fmt "E.5") 2/3)} → @code{"6.66667e-1"}@(linebreak)
 @code{((fmt "E15.5.4") 2/3)} → @code{"◦◦6.66667e-0001"}@(linebreak)
 @code{((fmt "EXE") #e-1e+100000 -1.0e+100000)} → @code{"-1.e+100000◦-inf.0"}@(linebreak)
 @code{((fmt "EXE") #e-1e-100000 -1.0e-100000)} → @code{"-1.e-100000◦-0.e+0"}@(linebreak)
 @code{((fmt "E15 5 3") 0)} → @code{"◦◦◦0.00000e+000"} ; all decimal figures 0.@(linebreak)
 @code{((fmt "e5 5 3") -0.0)} → @code{"-0.00000e+000"}@(linebreak)
 @code{((fmt "E15 5 3") (/ 0.0 0.0))} → @code{"◦◦◦◦◦◦◦◦◦+nan.0"}}

The numerical format-instructions
@literal{B}, @literal{O}, @literal{H} and @literal{=}
are padded according to the current
@seclink["Padding" "padding"] mode described elsewhere in this document.

@deffmt[#:tag "B" "B"]{

 Displays a real number in binary notation.}

@deffmt[#:tag "O" "O"]{

 Displays a real number in octal notation.}

@deffmt[#:tag "H" "H"]{

 Displays a real number in hexadecimal notation.}

@deffmt[#:tag "=" "="]{

 Displays a real number in decimal notation.}

@literal{B}, @literal{O}, @literal{H} and = convert the number to an exact one and
use @racket[number->string] to convert the absolute value and
display the result according to the current padding and sign mode.
The exceptional numbers @code{±inf.0} and @code{±nan.0} are treated specially.

@space-note

@code{((fmt "H") 20/31)} → @code{"14/1f"}@(linebreak)
@code{((fmt "D") 20/31)} → @code{"20/31"}@(linebreak)
@code{((fmt "H") -2.71)} → @code{"-ad70a3d70a3d7/4000000000000"}@(linebreak)
@code{((fmt "H") 2.71e-2)} → @code{"6f0068db8bac7/100000000000000"}@(linebreak)
@code{((fmt "H") #e2.71e-2)} → @code{"10f/2710"}
; 271 in the denominator is a coincidence.

Notice that @code{#x10f} = @code{271} and @code{#x2710} = @code{10000}
as shown in the following example:

@code{((fmt "=") #e2.71e-2)} → @code{271/10000}@(linebreak)
@code{((fmt "R8 H")  2.71e-200000)} → @code{"◦◦◦◦◦◦◦0"}@(linebreak)
@code{((fmt "R8 H") -2.71e-200000)} → @code{"◦◦◦◦◦◦-0"}@(linebreak)
@code{((fmt "R8 H")  2.71e2000000)} → @code{"◦◦+inf.0"}@(linebreak)
@code{((fmt "H") 0)} → @code{"0"}@(linebreak)
@code{((fmt "H") -0)} → @code{"0"} ; Because @code{(eqv? 0 -0)} → @code{#t}@(linebreak)
@code{((fmt "H") -0.0)} → @code{"-0"} ; Because @code{(eqv? 0.0 -0.0)} → @code{#f}

@subsection{Sign mode}
The sign mode is relevant for the numerical instructions @elemref["I" "I"],
@elemref["F" "F"], @elemref["E" "E"], @elemref["B" "B"], @elemref["O" "O"], @elemref["H" "H"] and
@elemref["="  "="].
When sign mode is off, positive numbers get no sign.
When sign mode is on, positive numbers, exact zero and @code{+0.0} get a plus sign.
Negative numbers always get a minus sign, @code{-0.0} included.
Instructions @elemref["I" "I"], @elemref["F" "F"] and @elemref["E" "E"]
may round the number such as to fit the width of the fraction.
If rounding a negative number yields zero, the minus sign is retained.
Notice that the @racketlink[exact-integer? "exact integer number"]
@tt{-0} has no sign. It is the same as @tt["+0"]
(in the sense of @racket[eq?]).
When a format-procedure is called from another format-procedure,
the former inherits the sign mode from the latter.
If the called procedure alters the sign mode,
this mode remains effective after return.
Instruction @elemref["$" "$"] can be used to restore the previous sign mode.

@deffmt["+"]{
 Switches sign mode on.}
@deffmt["-"]{
 Switches sign mode off.}

@deffmt[#:tag "$" "$ξ"]{
 Memorizes the current sign mode,
 executes instruction ξ and upon completion restores the memorized sign mode.}

@subsection{Tabulation}
Tabulator instructions reposition the write head within or beyond the end of the current line.
The position at the start of the current line has index 0.
Initially the current line starts at the very beginning of the output to be produced.
The newline instructions @elemref["/" "/"] and @elemref["|" "|"]
shift the start of the current line to the start of the new line.
New lines made in any other way (for example when part of a literal datum)
do not reposition the start of the current line.
They are considered to be part of the current line.
When the position is moved forward, existing output is not erased.
When the new position is beyond the end of the current line,
spaces are added at the end of the current line.
Placing the write head before the end of the current line does not erase output,
but allows subsequent output to replace previous output.
The instructions are effective even if the output device does not allow
reposition of the write head.
@red{Warning}: the tabulator is useful only with fixed width character fonts, such a Courier font.

@deffmt[#:tag "T" "Tν"]{

 Places the write head at position ν of the current line.}

@deffmt[#:tag ">" ">ν"]{

 Positions the write head forward relative to the current position.}

@deffmt[#:tag "<" "<ν"]{

 Positions the write head backwards relative to the current position.
 An exception is raised when an attempt is made
 to position the write head before the start of the current line.}

@deffmt[#:tag "&" "&"]{

 Positions the write head at the end of current line.}

@deffmt[#:tag "@" "@ξ"]{

 Memorizes the position of the start of the current line,
 executes instruction ξ and upon completion restores the memorized position.
 @red{Warning}:
 after restoring, new lines produced by ξ become part of the original current line.}

Examples: @space-note

@code{((fmt "T10 D T6 D T2 D &R4D") 1 2 3 4)} → @code{"◦◦3◦◦◦2◦◦◦1◦◦◦4"}@(linebreak)
@code{((fmt "*(T#D)") 1 1 4 4 3 3 5 5 2 2 6 6 7 7 0 0)} → @code{"01234567"}

@subsection[#:tag "condition" "Conditional instructions"]

@deffmt[#:tag "!" "!ξ"]{

 Executes instruction ξ only if there are more data.}

@deffmt[#:tag "?" "?ξ"]{

 Executes instruction ξ only if there are no more data.}

@deffmt[#:tag "Q" "Qξξ"]{

 Requires one datum, but does not consume it.
 If the datum is true, the first instruction is executed, else the second one.}

@deffmt[#:tag "sel" "{ξ ...+}"]{

 Requires and consumes a natural number.
 This number is used as index to select one instruction of @larger["ξ ..."],
 counting from 0. The other instructions are ignored.
 An exception is raised if the index is greater than or equal to
 the number of instructions in  @larger["ξ ..."].}

Examples: @space-note
@code{((fmt "!(*(D!X)/)") 1 2 3 4)} → @code{"1◦2◦3◦4\n"}@(linebreak)
@code{((fmt "!(*(D!X)/)"))} → @code{""}@(linebreak)
@code{((fmt "*({'zero' 'one' 'two'}!x)") 2 1 0)} → @code{"two◦one◦zero"}@(linebreak)
@code{((fmt "Q'True','False'S") #t)} → @code{"True"}
; Instruction @code{S} removes @code{#t} from the data.

@subsection[#:tag "iteration"]{Iterations}

@deffmt["*ξ"]{

 Repeated execution of ξ until no data remain.}

@deffmt[#:tag "νξ" "νξ"]{

 Instruction ξ is executed ν times.}

Examples: @space-note

@code{((fmt "R3 4D")     1 2 3 4)} → @code{"◦◦1◦◦2◦◦3◦◦4"}@(linebreak)
@code{((fmt "R3 *D")     1 2 3 4)} → @code{"◦◦1◦◦2◦◦3◦◦4"}@(linebreak)
@code{((fmt "R# #D") 3 4 1 2 3 4)} → @code{"◦◦1◦◦2◦◦3◦◦4"}

@deffmt[#:tag "_νμξξ" "_νμξξ" ]{

 Executes the first instruction ν times but after every μ times the second instruction is inserted.}
Examples:

@scheme[((fmt "_9.3'a' 'b'"))] → @code{"aaabaaabaaab"}

@scheme[((fmt "_8.3'a' 'b'"))] → @code{"aaabaaabaa"}

@space-note
@scheme[((fmt 'current "U_#3(DX)/|'fin'") (list 1 2 3 4 5 6 7 8))] → void, displays:@linebreak[]
@code{1◦2◦3◦}@linebreak[]
@code{4◦5◦6◦}@linebreak[]
@code{7◦8◦}@linebreak[]
@code{fin}@linebreak[]

@scheme[((fmt 'current "U_#3(DX)/|'fin'") (list 1 2 3 4 5 6 7 8 9))] → void, displays:@linebreak[]
@code{1◦2◦3◦}@linebreak[]
@code{4◦5◦6◦}@linebreak[]
@code{7◦8◦9◦}@linebreak[]
@code{fin}

@Interaction/no-prompt[
 (define v (for/list ((k (in-range 24))) (random)))
 ((fmt 'current "u_#4f8.3/|d") v "output to follow")]

Mark the vertical bar following the slash.
In the previous example it does not include an extra line-break before the output to
follow. In the following example it does insert a line-break.

@Interaction/no-prompt[
 (define v (for/list ((k (in-range 25))) (random)))
 ((fmt 'current "u_#4f8.3/|d") v "output to follow")]

@subsection[#:tag "compound"]{Compound instructions}

@deffmt[#:tag "simple-compound" "(ξ ...+)"]{

 Compound instruction. Useful for @seclink["condition" "conditions"],
 @seclink["iteration" "iterations"] and as argument of an instruction with one or more other
 instructions as arguments.}

@deffmt[#:tag "special-compound" "[ξ ...+]"]{

 Special compound instruction.
 The output of the instructions is gathered in a string which
 after completion of the compound instruction is added to
 the remaining data and becomes the first next datum.
 Each special compound instruction has its own offset for the tabulator.
 The square brackets are part of the instruction.
 They do not indicate that ξ ... is optional.
 In fact the ellipsis makes ξ ... optional.
 [] produces an empty string.
 @red{Warning}: new lines produced by ξ ... become part of the original current line.}

Examples: @space-note

@code{((fmt "!(*(D!X)/)") 1 2 3 4)} → @code{"1◦2◦3◦4\n"}@(linebreak)
@code{((fmt "L3 [*D] C20 D") 1 2 3 4)} → @code{"◦◦◦◦◦1◦◦2◦◦3◦◦4◦◦◦◦◦"}@(linebreak)
@code{((fmt "['123'/'456'] D T3 '|'"))} → @code{"123|456"}

@subsection{Miscellaneous instructions}

@deffmt[#:tag "M" "Mξ"]{

 Memorizes the current padding mode, field width, tabulator offset and sign mode,
 executes the instruction and restores the memorized state.
 @red{Warning}: new lines produced by ξ become part of the original current line.}

@deffmt[#:tag ":" ":"]{

 Exits from @seclink["compound" "a compound instruction"]
 or from a format-procedure or format-string called with instruction @elemref["K" "K"].
 At top level same as instruction @bold{;}.}

@deffmt[#:tag ";" ";"]{

 Exits from the top level format-procedure.}

@deffmt[#:tag "S" "S"]{

 Skips one datum.}

@deffmt["~"]{

 Positions the write head at the end of the current line and
 writes all remaining data separated by spaces and terminated by a newline.
 Same as @code{"!(&n*(w!x)/)"}.
 Usually it is wise to write @code{"&x~"} or @code{"&|~"}
 in order to separate the remaining output from output already produced
 and to avoid writing over already generated output.}

@deffmt[#:tag "J" "J"]{

 No operation.}

@deffmt[#:tag "G" "G"]{

 Consumes a datum which must be a natural number(exact-nonnegative-integer) or @code{#f}.
 It is supposed to be a time measured in seconds from the platform specific starting time.
 @code{#f} is for the current time.
 The time is displayed as: @literal["DDD,◦dd◦MMM◦yyyy◦hh:mm:ss◦±hhmm"],
 exactly 31 characters.
 Instruction G limits the argument to less than 2@superscript{32}.}

@tabular[
 #:sep @hspace[1]
 (list
   (list @code["DDD"]
     "First three letters of the name of the day of the week.")
   (list @code["dd"]
     "Two decimal figures for the number of the day of the month.")
   (list @code["MMM"]
     "First three letters of the name of the month.")
   (list @code["yyyy"]
     "Four decimal figures of the year")
   (list @code["hh"]
     "Two decimal figures for the hour of the day on 24 hour basis.")
   (list @code["mm"]
     "Two decimal figures for the minute within the hour.")
   (list @code["ss"]
     "Two decimal figures for the second within the minute (leap second included)")
   (list @code["±hhmm"]
     "Time zone, hours and minutes, sign followed by four decimal figures."))]

Examples (assuming Windows XP or Windows 7 in time zone +0100)

@space-note

@code{((fmt "G") 0)} → @code{"Thu,◦01◦Jan◦1970◦01:00:00◦+0100"}@(linebreak)
@code{((fmt "^'0' G"))}  Same as: @code{((fmt "G") 0)}@(linebreak)
@code{((fmt "G") (sub1 (expt 2 31)))} → @code{"Tue,◦19◦Jan◦2038◦04:14:07◦+0100"}@(linebreak)
@code{((fmt "G") #f)}  Same as: @code{((fmt "G") (current-seconds))}@(linebreak)
@code{((fmt "^'#f' G"))}  Same as: @code{((fmt "G") #f)}

The following produces an exception:

@Interaction[
 ((fmt "G") (expt 2 32))]

@subsection[#:tag "unfolding"]{Unfolding}

Unfolding means that the elements of a vector or immutable list will be treated as separate data.
These data are preceded by the number of elements.
This exact non-negative integer number can be used as a repetition-count with instruction
@elemref["νξ" "νξ"].
A structure (that satisfies predicate @scheme[struct?]) is first converted to a vector and
the latter is unfolded.
A box is unfolded by taking it's contents and adding 1 as the first next datum.
If the datum is a mutable pair or an improper list,
the chain of mutable and immutable pairs is unfolded as if it were flattened.
Any other type of object remains in the list of data as it is and
the exact number one is consed to the data as the first next datum.
Unfolding is protected against cycles such as in a mutable pair whose @scheme[mcdr] is the same pair
or a vector containing itself as an element.
An element that causes a cycle is not unfolded. 

@deffmt[#:tag "U" "U"]{

 Consumes a datum and unfolds it. Examples: 

 @scheme[((fmt "U~") '())] → @code{"0\n"}@(linebreak)
 @scheme[((fmt "U~") '(a b c))] → @code{"3 a b c\n"}@(linebreak)
 @scheme[((fmt "U~") '(() () ()))] → @code{"3 () () ()\n"}@(linebreak)
 @scheme[((fmt "U~") '(a b . c))] → @code{"3 a b c\n"}@(linebreak)
 @scheme[((fmt "U~") '((a b c) (d e f) (g h i)))] → @code{"3 (a b c) (d e f) (g h i)\n"}@(linebreak)
 @scheme[((fmt "U~") #(a b c))] → @code{"3 a b c\n"}@(linebreak)
 @scheme[((fmt "U~") #((a b c) (d e f) (g h i)))] → @code{"3 (a b c) (d e f) (g h i)\n"}

 @Interaction/no-prompt[
 (code:comment @#,t{Pad individual elements of a list.})
 (code:comment @#,t{L3 : left allignment in fields of 3 characters.})
 (code:comment @#,t{U#D : unfold and display each element.})
 (define a '("   a    " "    b" "c    "))
 ((fmt "L3U#D") a)]

 @Interaction/no-prompt[
 (define p (mcons 'a 'b))
 (set-mcdr! p p)
 ((fmt "U~") p)]

 @Interaction/no-prompt[
 (define v (vector 1 2))
 (vector-set! v 1 v)
 ((fmt "US~") v)]}

@deffmt[#:tag "V" "V"]{

 Consumes and recursively unfolds the next datum in depth first order. Examples:

 @scheme[((fmt "V~") '())] → @code{"0\n"}@(linebreak)
 @scheme[((fmt "V~") '(a b c))] → @code{"3 a b c\n"}@(linebreak)
 @scheme[((fmt "V~") '(() () ()))] → @code{"0\n"}@(linebreak)
 @scheme[((fmt "V~") '(a b . c))] → @code{"3 a b c\n"}@(linebreak)
 @scheme[((fmt "V~") '((a b c) (d e f) (g h i)))] → @code{"9 a b c d e f g h i\n"}@(linebreak)
 @scheme[((fmt "V~") #(a b c))] → @code{"3 a b c\n"}@(linebreak)
 @scheme[((fmt "V~") #((a b c) (d e f) (g h i)))] → @code{"9 a b c d e f g h i\n"}

 @Interaction[
 (define v (vector 'a 'b 'c))
 (vector-set! v 1 v)
 ((fmt "V~") v)]
 because the second element (index 1) causes a cycle.}

@deffmt[#:tag "Z" "Z"]{

 Consumes all remaining data and recursively unfolds the list of these data.}

@deffmt[#:tag "Y" "Y"]{

 Consumes a datum, which must be a number.
 It is decomposed into its real and imaginary part.
 These are consed as two real numbers to the remaining data,
 the real part becoming the first next datum, the imaginary part the second one.}

@deffmt[#:tag "%" "%"]{

 Consumes a datum, which must be a rational number.
 In Racket all real numbers, @code{+nan.0}, @code{+inf.0} or @code{-inf.0} excluded,
 are rational too.
 The number is decomposed into its numerator and denominator.
 These are consed as two exact integer numbers to the remaining data,
 the numerator becoming the first next datum, the denominator the second one.
 The sign is associated with the numerator, the denominator always being positive.
 Zero, @code{+0.0} and @code{-0.0} included, are treated as @code{0}.}

@deffmt["\\"]{

 A single back-slash, but within a string an escaping back-slash is required.
 Consumes a datum, which must be a number.
 The number is consumed and its @code{magnitude} and @code{angle} are added to the remaining data.
 The magnitude and angle of exact zero are zero.}

Examples: @space-note

@code{((fmt "U#(DX)") '(a b c d))} → @code{"a◦b◦c◦d◦"}@(linebreak)
@code{((fmt "U*(DX)") '(a b c d))} → @code{"4◦a◦b◦c◦d◦"}@(linebreak)
@code{((fmt "YF.3+F.2'i'") -12.34+56.78i)} → @code{"-12.340+56.78i"}@(linebreak)
@code{((fmt "%d'/'d") -0.0)} → @code{0/1}@(linebreak)
@code{((fmt "\\f.4xf.4") 3+4i)} → @code{"5.0000 0.9273"}

@code{((fmt 'current "*(%R20DN'/'L20D/)") 110/333 -1/3 0.75 -0.75 (/ 1.0 3))}
displays:

@verbatim{
 ◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦110/333◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦
 ◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦-1/3◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦
 ◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦3/4◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦
 ◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦-3/4◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦◦
 ◦◦◦◦6004799503160661/18014398509481984◦◦◦}

where @code{18014398509481984} = @code{(expt 2 54)}.

@Interaction/no-prompt[
 ((fmt "\\dxd") 0)
 ((fmt "\\dxd") 0.0)
 ((fmt "\\dxd") -0.0)]

@subsection{Procedure calls}

@deffmt[ "λ"]{

 Consumes the next datum, which must be a procedure.
 The procedure must accept one argument,
 for which it receives the list of remaining data.
 It must return a list. This list replaces the list of remaining data.

 The procedure is called with a continuation barrier.
 This prohibits re-entry by means of a continuation in the body of the procedure.
 Allowing re-entrance could lead to unexpected results
 when the called procedure returns a second time after the state of the format-procedure has been
 altered or the format-procedure already has finished.

 The called procedure may alter parameters.
 If the @italic{@code{port}} or @italic{@code{port-arg}}-argument of the format-procedure is
 @racket['current] or @racket['cur] and
 the procedure alters parameter @racket[current-output-port],
 the format-procedure sticks to the output-port as was found in this parameter
 before the alteration.}

@deffmt[#:tag "K" "K"]{

 Consumes the next datum, which must be a format-procedure or a format-string.
 If a format-string is given, it is checked and translated,
 but repeated parsing and translation is avoided.
 See section @seclink["avoid" "Repeated parsing and translation avoided"].
 If a format-procedure is given, its port is ignored.
 The instructions are executed as part of the calling format-procedure.
 The called format-procedure inherits the padding mode, tabulator and sign mode from the caller.
 If it alters the state, the alterations remain effective after return.
 Instructions @elemref["A" "A"], @elemref["@" "@"], @elemref["$" "$"] and @elemref["M" "M"]
 can be used to preserve the padding mode, tabulator offset and sign mode.}

@section{Reuse of format-procedures}

@racketblock[
 (define a (fmt "X'billy'XD"))
 (define b (fmt "X'minny'XD"))
 (define c (fmt "*(" a "!" b ")"))
 (c 1 2 3 4 5)] → @code{"◦billy◦1◦minny◦2◦billy◦3◦minny◦4◦billy◦5"}

When format-procedure c is constructed, format-procedures a and b are inserted.
The result is the same as with @code{"*((X'billy'XD)!(X'minny'XD))"},
but without parsing the inserted parts again.
Notice that the compound instruction in the definition of c
is distributed over more than one argument.
Left parentheses may be balanced by format-string arguments yet to follow.

@section{Elaborated examples}

@subsection{Printing a bill}

Procedure @code{print-bill} accepts a list of entries, each entry being a list of three arguments:
the name of an article, how many pieces of this article are bought and the price per piece.
The purpose of the procedure is to display a detailed bill.

@racketblock[
 (define print-bill
   (let
       ((line "N40('-')/")
        (headers "R10'article','number','price pp','total'/")
        (data "R10U#(USUS2D2F10.2/)")
        (code:comment @#,t{"US" means: unfold and skip element count})
        (grand-total "R30'grand total'F10.2/"))
     (let
         ((fmt-proc
            (fmt "/" line headers line data line grand-total line 'current)))
       (lambda (table)
         (let*
             ((totals (map (λ (x) (* (cadr x) (caddr x))) table))
              (grand-total (apply + totals)))
           (fmt-proc (map list table totals) grand-total))))))
 
 (print-bill '((chair 4 50) (table 1 100) (pillow 4 10)))]

→ void and displays:

@verbatim{
----------------------------------------
   article    number  price pp     total
----------------------------------------
     chair         4     50.00    200.00
     table         1    100.00    100.00
    pillow         4     10.00     40.00
----------------------------------------
                   grand total    340.00
----------------------------------------}

@subsection{Triangle of Pascal}

In this example padding C is used in order to form a triangle of Pascal
with its base at the bottom and all other lines centred above the bottom line.

@racketblock[
 (define binomials
   (let
       ((fmt-row (fmt "R5 D US C4 [*D] C45 D"))
        (fmt-table (fmt "/U#(D/)" 'current)))
     (define (make-next-row order prev-row)
       (list->vector
         (cons 1
           (let loop ((j 1))
             (if (> j order) '(1)
               (cons
                 (+ (vector-ref prev-row (sub1 j)) (vector-ref prev-row j))
                 (loop (add1 j))))))))
     (lambda (n)
       (fmt-table
         (let loop ((order 0) (row #1(1)))
           (cons (fmt-row order row)
             (if (>= order n)  '( )
               (loop (add1 order) (make-next-row order row)))))))))
 
 (binomials 9)]

→ void and displays:

@verbatim{
 0                      1                      
 1                    1   1                    
 2                  1   2   1                  
 3                1   3   3   1                
 4              1   4   6   4   1              
 5            1   5  10  10   5   1            
 6          1   6  15  20  15   6   1          
 7        1   7  21  35  35  21   7   1        
 8      1   8  28  56  70  56  28   8   1      
 9    1   9  36  84  126 126 84  36   9   1  }

@section[#:tag "synopsis"]{Synopsis}

@tabular[
 #:sep @hspace[3]
 (list
   (list @elemref["A"]{A} "Preserve padding")
   (list @elemref["B"]{B} "Binary numerical format")
   (list @elemref["C"]{C} "Centred padding")
   (list @elemref["D"]{D} "Display")
   (list @elemref["E"]{E} "Scientific numerical format")
   (list @elemref["F"]{F} "Decimal expansion numerical format")
   (list @elemref["G"]{G} "Date and time")
   (list @elemref["H"]{H} "Hexadecimal numerical format")
   (list @elemref["I"]{I} "Integer numerical format")
   (list @elemref["J"]{J} "No operation")
   (list @elemref["K"]{K} "Call fmt-procedure or format-string")
   (list @elemref["L"]{L} "Left padding")
   (list @elemref["M"]{M} "Preserve state (padding, sign-mode and tabulator)")
   (list @elemref["N"]{N} "No padding")
   (list @elemref["O"]{O} "Octal numerical format")
   (list @elemref["P"]{P} "Print")
   (list @elemref["Q"]{Q} "Conditional")
   (list @elemref["R"]{R} "Right padding")
   (list @elemref["S"]{S} "Skip")
   (list @elemref["T"]{T} "Tabulator")
   (list @elemref["U"]{U} "Unfold")
   (list @elemref["V"]{V} "Unfold recursively")
   (list @elemref["W"]{W} "Write")
   (list @elemref["X"]{X} "Space")
   (list @elemref["Y"]{Y} "Decompose complex number")
   (list @elemref["Z"]{Z} "Unfold recursively")
   (list @elemref["="]{=} "Decimal numerical format")
   (list @elemref["simple-compound"]{(...)} "compound instruction")
   (list @elemref["special-compound"]{[...]} "compound instruction")
   (list @elemref["simple-literal"]{'κ ...'} "Literal data")
   (list @elemref["compound-literal"]{^'κ ...'} "Literal data")
   (list @elemref["%"]{%} "Decompose in numerator and denominator")
   (list @elemref["\\"]{\} "Decompose in magnitude and angle")
   (list @elemref["/"]{/} "Newline")
   (list @elemref["|"]{|} "Newline but not double")
   (list @elemref["~"]{~} "Display all remaining data")
   (list @elemref["*ξ"]{*ξ} "Repeat until no more data")
   (list @elemref["νξ"]{νξ} "Repeat ν times")
   (list @elemref["!"]{!} "When more data left")
   (list @elemref["?"]{?} "Unless more data left")
   (list @elemref["+"]{+} "Sign mode on")
   (list @elemref["-"]{-} "Sign mode off")
   (list @elemref["$"]{$} "Preserve sign mode")
   (list @elemref[":"]{:} "Local exit")
   (list @elemref[";"]{;} "Top level exit")
   (list @elemref["&"]{&} "Tabulate to end of line")
   (list @elemref["@" "@"] "Preserve tabulator")
   (list @elemref["<"]{<} "Relative tab backward")
   (list @elemref[">"]{>} "Relative tab forward")
   (list @elemref["_νμξξ"]{_νμξξ} "Iteration")
   (list @elemref["λ"]{λ} "Call procedure")
   (list @elemref["sel"]{@tt["{ξ...}"]} "Instruction selector"))]

@larger{@larger{@@bold{The end}}}