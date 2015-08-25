# mixfix-clj

Provides mixfix syntax for clojure language.

It simply allows writing clojure forms like this:

```clojure

(defn myfun [x y] 
   (if x < 2 then x + y - 1 else (x + y) * 2))

```

But probably it is more useful for easy EDSL syntax definitions.

## Usage

Import the library:

```clojure
(ns sample.mixfix
  (:require [clojure.tools.mixfix :as m]))
```

Now we define some operators:

```clojure

(m/op 400 + [[x] + [X]])
(m/op 400 - [[x] - [X]])
(m/op 500 * [[x] * [X]])
(m/op 500 / [[x] / [X]])
(m/op 200 or [[x] or [X]]) 
(m/op 300 and [[x] and [X]]) 
(m/op 400 = [[x] is [x]])
(m/op 100 s [[x] [X]])

(defn s [a b] [a b])

(m/op 100 when [if [X] then [x]])
(m/op 110 if [if [X] then [x] else [x]])
```

And now use them:

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
3. head symbol for clojure form the operator will be converted too
4. syntax picture

Syntax picture is a vector of symbols interleaved with another single 
element vectors specifying syntax holes. 

In the hole definition may be either:

  * number - specifies any precedence explicitly 
  * X - means precedence is bigger than precedence of the operator in this 
    position, literally just an increment
  * x  - means precedence is the same as the operator's one

So this is it, now we can use mixfix operatos. It is converted into plain 
clojure form using `%` macros. It walks through all sub-forms and parses 
their content.

```clojure

(m/% 2 + 2)       ; ==> 4
(m/% 2 + 2 - 2)  ; ==> 4
(m/% 2 + (2 - 2)); ==> 2
(m/% let [x 2 y 2] (x + y - 2)); ==> 4
(m/% if 2 is 3 then if 3 is 4 then 5 else 6); ==> 4

;; it also composes with plain clojure forms: 

(m/% 2 + (- 2 2)) ; ==> 2

```

Plain clojure forms may be also converted back into mixfix syntax.


```clojure

(m/print (+ (+ (+ 1 2) 3) 4) 5)); ==> (1 + 2 + 3 + 4 + 5)

``` 

## Syntax scopes

If some operators belong only to some EDSL (passed as parameters to some macros) 
they may be assigned to some named scope. This scope can be used in parse function 
to convert it to plain clojure form for further handling by EDSL implementation. 

Such scope is defined using `clojure.tools.mixfix.core/declare-lang` macros. The 
first parameter is a name of the scope. The second optional parameter is another 
scope where initial operators' definitions are to be copied from. It creates 
a variable with the same name which is used for referencing the scope. It may be 
passed as the optional first argument in `op` directives. And it may be passed to 
`clojure.tools.mixfix.core/parse` function via dynamic variable 
`clojure.tools.mixfix.core/*lang*` form the macro receiving EDSL expressions as 
parameters. Variable `clojure.tools.mixfix.core/global` is used as default scope. 
There is also corresponding macros `%*` with additional parameter for the scope 
specification.

## Limitations

If some library implements its own EDSL syntax parser it will not compose well with 
this library. An example is `clojure.test/is`. It may take expected exception thrown 
specification with `thrown?` keyword. It is not macros and it is not function. It is 
just a part of another language `clojure.test/is` can understand. On the other hand 
mixfix-clj doesn't know anything about this keyword. So it will report parser error. 
It could ignore this and leave the form as is but it would significantly reduce 
diagnostic capabilities. There is a macros for registering such kind of keywords 
(namely `clojure.tools.mixfix.core/reg-sym`). But even registered it won't work 
anyway. Not the problem is ordering of macros expansion. But if the library also 
uses mixfix-clj for syntax parsing it should work without problems.

## License

Copyright © 2015 Vitaliy Akimov

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
