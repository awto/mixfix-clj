(ns clojure.tools.mixfix.core
  (:require [clojure.tools.mixfix.parser :as r]
            [clojure.tools.mixfix.printer :as p]
            [clojure.walk :as w]))

(defmacro declare-lang 
  "Defines a name which may be used to specify scopes of mixfix operators.
   The second optional arguments specifies which language to copy initial 
   definitions from. The definitions are copied only once."
  ([nm] `(def ~nm (atom {})))
  ([nm par] `(def ~nm (atom @~par))))

(declare-lang global)

(defn- picture-item?
  [item] (or (symbol? item)
             (and 
               (vector? item) 
               (= 1 (count item))
               (let [[i] item]
                 (or 
                   (number? i)
                   (#{"X" "x"} (name i))
                   )))))

(defn- picture-arity [picture] (count (filter number? picture)))

(defn- lang-def? [d] (and (:read d) (:write d)))

(defn- compile-picture 
  [prec picture] {:pre [(number? prec) (every? picture-item? picture)]}
  (let [nprec (inc prec)]
    (for [i picture]
      (cond
        (symbol? i) i
        (vector? i) (let [[mod] i]
                      (if (number? mod) mod
                        (case (name mod)
                          "X" nprec
                          "x" prec)))))))

(defn- add-op [table prec symbol pict]
  (let [cpict (compile-picture prec pict)]
    (-> table 
      (update-in [:read prec] (fnil conj #{}) [cpict symbol])
      (assoc-in [:write symbol (picture-arity cpict)] 
                [prec (p/compile-pict cpict)])
      (assoc-in [:parser] nil))))

(defn mk-op [lang prec picture symbol]
  (swap! lang add-op prec picture symbol))

(defmacro op
  "Defines mixfix operator. First optional argument is a name for the 
   operator's scope. The second is precedence level of the operator. The bigger 
   the number the tightly the operator binds. The third is resulting form head
   symbol. And the last one is a mixfix picture of the operator."
  ([lang prec symbol picture] `(mk-op ~lang ~prec '~symbol '(~@picture)))
  ([prec symbol picture] `(mk-op global ~prec '~symbol '(~@picture))))

(def ^:dynamic *locals*
  "Specifies locally defined symbols, 
   so they are not considered to be syntax part."
  #{})

(def ^:private specials (atom #{}))

(defmacro reg-sym
  "Adds a symbol to a list of known reserved words. For which are not local or
   cannot be resolved (some other library EDSL symbols)."
  [n] (swap! specials conj n) 'nil)

(reg-sym do)

(defn prim? [v] ((some-fn (complement symbol?)
                          (partial contains? *locals*)
                          (partial resolve)) v))

(def prim (r/guard r/any prim?))
(defn clj-mixfix
   "builds mixfix expression table with clojure syntax"
   [table]
   (r/exp-table table prim))

(def ^:dynamic *lang* 
  "specifies current language"
  global)

(defn- get-parser []
  (let [ldef @*lang*]
    (or (:parser ldef)
        (let [res (clj-mixfix (:read ldef))]
          (swap! *lang* assoc :parser res)
          res))))

(defn parse
  "Parses 1 level syntax with mixfix operators. Returns plain clojure form
   without them if succeed."
  [col]
    (if (< (count col) 2) 
      (apply list col) 
      (let [parser (get-parser)
            r (r/run parser col)]
        (cond 
          (empty? r) (if (every? prim? col)
                       col
                       (throw (IllegalArgumentException. 
                                (format "no parse for: %s" col))))
          (= 1 (count r)) (first r)
          :else (throw (IllegalArgumentException. 
                         (format "%s is ambiguous, options are: %s" col r)))))))

(defn parse-all
  "Deep version of `parse`. Parses also inside sub-forms."
  [col] 
  (w/prewalk #(if (list? %) (parse %) %) col))

(defmacro %1
  "Shallow version of %."
  [& col]
  (let [f (binding [*locals* (into @specials (keys &env))]
            (parse col))]
    `(~@f)))

(defn propagate-%
  ([v] (w/prewalk (fn [i] (if (list? i) (cons (var %1) i) i)) v))
  ([lang v] (w/prewalk (fn [i] (if (list? i) (list* '%1* lang i) i)) v)))

(defmacro %
  "Transforms form and its subforms into clojure syntax without mixfix ops and
   evaluate it."
  [& f]
  `(%1 ~@(propagate-% f)))

;;; specifying custom language
(defmacro %1*
  "Shallow version of %*."
  [lang & col]
  (let [f (binding [*locals* (into @specials (keys &env))
                    *lang* @(resolve lang)]
            (parse col))]
    `(~@f)))

(defmacro %*
  "Transforms form and its subforms into clojure syntax without mixfix ops. 
   First argument specifies language name."
  [lang & f]
  `(%1* ~lang ~@(propagate-% lang f)))

(defn to-mixfix-1
  "Reverse of %. Takes a plain clojure form and transforms it to the one with
   mixifix operators."
  [col] (->> col (p/add-ops (:write @*lang*)) (apply list)))

(defn to-mixfix [col] (w/prewalk 
                        #(if (and (list? %) (> (count %) 1)) 
                             (to-mixfix-1 %) %) 
                        col))

nil

