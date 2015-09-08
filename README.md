# mixfix-clj

Provides mixfix syntax for Clojure language.

It simply allows writing Clojure expressions like this:

```clojure

(defn myfun [x y] 
   (if x < 2 then x + y - 1 else (x + y) * 2))

```

You can also easily define nice syntax of your next EDSL.

For example some SQL-like

```clojure
(exec (select * from table1, table2 where col1 < col2 group by col1, col2)) 
``` 

The `exec` there is user-defined macros. It uses this library 
`clojure.tools.mixfix.core\parse` function to convert concrete SQL-like
syntax into abstract syntax tree. This tree is plain Clojure form, and it easy 
to analyze or execute or convert into some DBMS query syntax using standard 
Clojure means, like `clojure.walk`.

This is just simple macros, no extra build steps and no extra build steps 
are required.

## Usage

Install it using Clojars:

[![Clojars Project](http://clojars.org/mixfix-clj/latest-version.svg)](http://clojars.org/mixfix-clj)

Import the library:

```clojure
(ns sample.mixfix
  (:require [clojure.tools.mixfix :as m]))
```

This version doesn't support ClojureScript.

Now define some operators:

```clojure

(m/op 400 + [[] + [+]])
(m/op 400 - [[] - [+]])
(m/op 500 * [[] * [+]])
(m/op 500 / [[] / [+]])
(m/op 200 or [[] or [+]]) 
(m/op 300 and [[] and [+]]) 
(m/op 400 = [[] is []])

(defn s [a b] [a b])

(m/op 100 if [if [+] then []])
(m/op 110 if [if [+] then [] else []])
```

And use them:

```clojure


(m/% 2 + 2) ; ==> 4
(m/% 2 - 2 - 2)  ; ==> -2
(m/% 2 - (2 - 2)); ==> 2
(m/% let [x 2 y 2] (x + y - 2)); ==> 2
(m/% if 2 is 2 then if 3 is 4 then 5 else 6); ==> 6
(m/% 2 + (- 2 2)) ; ==> 2

```

The arguments for `op` are:

  1. optional language name
  2. precedence level, the bigger the number the tightly the operator binds
  3. head symbol for clojure application list the operator will be converted to
  4. syntax picture

Syntax picture is a vector of symbols interleaved with another vectors 
specifying syntax holes. The hole definition vector may contain various options. 
In the current version they may be either: 

  * `empty` - means same precedence level as its operator
  * `number` - specifies any precedence explicitly
  * `+` - precedence is the same as the operator's one
  * `assoc` - will unwrap sub-form if it has same head symbol as the operator
  * `id <value>`  - for assoc operators will treat as identity for the 
    operation, by default `nil`.

So this is it. Mixfix operators are converted into plain clojure application 
form using ` %` macros. It walks through all sub-forms and parses their content 
too. There is also shallow version `%1` which parses only a single level.

```clojure

(m/% 2 + 2)       ; ==> 4
(m/% 2 + 2 - 2)  ; ==> 4
(m/% 2 + (2 - 2)); ==> 2
(m/% let [x 2 y 2] (x + y - 2)); ==> 4
(m/% if 2 is 3 then if 3 is 4 then 5 else 6); ==> 4

;; it also composes with plain clojure application forms: 

(m/% 2 + (- 2 2)) ; ==> 2

```

There is also `clojure.tools.mixfix/defn` macros which simply redirects to 
`clojure.core/defn` but wraps arguments with operators parsing macros.

Plain clojure application may be also converted back into mixfix syntax.


```clojure

(m/to-mixfix (- (+ (- (+ 1 2) 3) 4) 5))); ==> (1 + 2 - 3 + 4 - 5)

``` 

It matches syntax definitions by arity, so if there are ambiguous symbol name
plus arity it may fail to do this property.

## Associative operators

Clojure often permits many arguments in an expression for typically binary 
operators, such as `clojure.core/+` etc. The library can handle such operators 
too. For this in the vector of syntax hole definition add  `assoc` option and 
optionally identity symbol for that operation. For example for addition:

```clojure

(op 400 + [[assoc id 0] + [+]])

; now + will be parsed into single `+` form

(macroexpand '(r/% 1 + 2 + 3)) ; ==> (+ 1 2 3)
(macroexpand '(r/% 0 + 1)) ; ==> (+ 1)

```

This isn't useful much for arithmetic operators unless generated code must
be readable. But it is useful for example for `clojure.core/list`. 

## Interleaving with clojure applications
There are two ways to use plain clojure application forms inside mixfix syntax. 
By default there is an operator for space or comma (and it is the only 
predefined operator in this version of the library).

```clojure

(op 1000 form [[assoc] [+]])

```

The library provides an auxiliary macros `clojure.tools.mixfix.core/form` it 
simply splices its arguments in a list without doing with them anything. So as 
a result it will be plain clojure application. For example

```clojure

(m/op 200 if [if [+] then []])

(clojure.walk/macroexpand-all '(% if = 2 2 then :t)) 
; ==> (if (= 2 2) (do :t))

```

This option may be not convenient to detect syntax error sometimes, for example
if we define "==" operator but accidently use "=" instead. 

```clojure
(clojure.walk/macroexpand-all '(% if 2 = 2 then :t)) 
; ==> (if (2 = 2) (do :t))
```

And clojure will complain about "2" isn't function, and this may be confusing. 
This is an issue only for operators clashing with predefined function or macros 
names in scope. The library will only accept them if it can `clojure.core/resolve` 
all the items of the list. It is also possible to disable such behavior by 
removing such operator with: 

```clojure
(m/remove-op form)
```

In this case clojure plain application can still be parsed but it must be in 
parens. This is a kind of parenthesis symbols overloading. They may be used for 
grouping mixfix sub-expressions and for specify clojure applications.

```clojure
(clojure.walk/macroexpand-all '(% if (= 2 2) then :t)) 
; ==> (if (2 = 2) (do :t))
```

After the library detected parser error within parens it will try to interpret 
them as a plain clojure list. Library will conclude the form is ok if all 
symbols there can be resolved. This behavior may be also turned off using 
`clojure.tools.mixfix.core/*clojure-apps*` dynamic variable if your EDSL 
doesn't need it. After only mixfix predefined operators can be present in 
parsed expression.

Another thing may be useful for custom EDSL, is `clojure.tools.mixfix/*locals*` 
variable, which is a set of symbols bound to some local variable in a form 
currently parsed. By default it is inited from &env parameter, but for custom 
EDSL, if it has some custom bound names they must be added to the set.

## Syntax scopes

If some operators belong only to some EDSL (passed as parameters to some macros)
they may be assigned to some named scope. This scope can be used in parse 
function to convert it to plain clojure form for further handling by EDSL 
implementation. 

Such scope is defined using `clojure.tools.mixfix.core/declare-lang` macros. The
first parameter is a name of the scope. The second optional parameter is another 
scope where initial operators' definitions are to be copied from. It creates 
a variable with the same name which is used for referencing the scope. It may be
passed as the optional first argument in `op` directives. And it may be passed 
to `clojure.tools.mixfix.core/parse` function via dynamic variable 
`clojure.tools.mixfix.core/*lang*` form the macro receiving EDSL expressions as 
parameters. Variable `clojure.tools.mixfix.core/global` is used as default
scope. There is also corresponding macros `%*` with additional parameter for the
scope specification.

For example defining SQL-like syntax:

```clojure
(r/declare-lang sql)
(r/op sql 100 select [select [+] from [+] where [+] group by [+]]) 
(r/op sql 100 select [select [+] from [+] where [+]]) 
(r/op sql 100 select [select [+] from [+]]) 
(r/op sql 200 list [[assoc] [+]])
(r/op sql 150 = [[+] = [+]])
(r/op sql 150 < [[+] < [+]])
; ......

```

It looks a bit verbose, especially if the language size will grow. But, since
`op` there is only a macros (not clojure syntax part), it may be easily 
generated. Or some next version will provide picture syntax for this.

These syntax further is parsed into AST with `clojure.tools.mixfix.core/parse`
function with `clojure.tools.mixfix.core/*lang*` variable bound to `sql` 
variable.

## Limitations

If some library implements its own EDSL syntax parser it will not compose well 
with this library. An example is `clojure.test/is`. It may take expected 
exception thrown specification with `thrown?` keyword. It is not a macros and 
it is not a function. It is just a part of another language `clojure.test/is` 
can understand. On the other hand mixfix-clj doesn't know anything about this 
keyword. So it will report parser error. It could ignore this and leave the 
form as is but it would significantly reduce diagnostic capabilities. There is 
a macros for registering such kind of keywords (namely 
`clojure.tools.mixfix.core/reg-sym`). But even registered it won't work anyway. 
Not the problem is ordering of macros expansion. But if the library also uses 
mixfix-clj for syntax parsing it should work without problems.

At the moment there is no namespaces support for operator's part. They are 
simply compared by `clojure.core/name`. But their support is planned for some
next version. This will be another level of operations scoping.

## License

Copyright © 2015 Vitaliy Akimov

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
