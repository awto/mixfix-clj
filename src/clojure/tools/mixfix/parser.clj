(ns clojure.tools.mixfix.parser
  "Parsing combinators for ambiguous grammars with left recursion.

  Based on Johnson's 'Memoization in Top-Down Parsing'"
  (:refer-clojure :exclude [list seq]))

(def ^:dynamic *enableTrace*
  "turns on/off traces specified with `trace` combinator" 
  false)

(def zero 
  "always fails"
  (fn [pos inp cont] ()))

(defn- parser? [v] (fn? v))

(defn trace 
  "Output debugging infomation during entering 
   and exiting parser's function."
  [name this] {:pre [(parser? this)]}
  (if *enableTrace*
    (fn [pos inp cont]
      (let [tok (get inp pos)]
        (println "> " name pos tok)
        (this pos inp
              (fn [npos nval] 
                (println "< " name pos tok nval)
                (cont npos nval)))))
    this))

(defn once 
  "For ambiguous parser returns only its first alternative if there is any.
   Uses exceptions to avoid next alternatives calculation."
  [this] {:pre [(parser? this)]}
  (fn [p i c] 
    (let [res (atom nil)]
      (try 
        (this p i 
              (fn [np v] 
                (reset! res [np v])
                (throw (ex-info "Exit" {:type :exit})
                )))
        (catch clojure.lang.ExceptionInfo e
          (when (-> e ex-data :type (= :exit))
            (apply c @res)))))))

(defn return
  "Returns parser which doesn't consume anything and 
   always returns `val` (monadish return)."
  [val] (fn [pos inp cont] (cont pos val)))

(defn guard 
  "filters out values where `pred` returns false"
  [this pred] {:pre [(parser? this) (fn? pred)]}
  (fn [pos inp cont]
    (this pos inp 
          (fn [npos val] 
            (when (pred val) (cont npos val))))))

(defn any
  "Consumes single token and returns it."
  [pos inp cont]
  (when (< pos (count inp)) 
    (cont (inc pos) (nth inp pos)))) 

(defn tok
  "If without arguments it returns parser consuming any token, 
   if with a single argument it will consume only tokens equal to it."
  ([] any)
  ([t] (guard any (partial = t))))

(defn sym 
  "parses clojure symbol token by its name (ignores namespace)"
  [t]
  (trace (format "name-%s" t) 
         (guard any 
                #(and (symbol? %) (= (name t) (name %))))))

(def pos 
  "Returns current position."
  (fn [p i c] (c i p)))

(def eof
  "Check it is end of file now."
  (fn [p i c]
    (when (= (count i) p)
       (c i p))))

(defn alt 
  "Grammar's alternatives. They may be ambiguous."
  [& opts] {:pre [(every? parser? opts)]}
  (fn [pos inp cont]
    (doseq [x opts] 
      (x pos inp cont))))

(defn seq 
  "Sequence of parsers, returns list of their resulting values"
  ([] (return []))
  ([col] {:pre [((some-fn parser? (partial every? parser?)) col)]}
    (if (coll? col)
      (if (empty? col)
        (return [])
        (let [[h & t] col
              ts (apply seq t)] ;TODO: maybe trampoline
          (fn [pos inp cont] 
            (h pos inp
               (fn [hpos hval] 
                 (ts hpos inp 
                     (fn [tpos tval] 
                       (cont tpos (cons hval tval)))))))))
      (seq [col])))
  ([v & other]
    (seq (cons v other))))

(defn join
  "Monadish join. Useful for adding context depedency, but very hard to use
   for ambigous grammars. So it is better to avoid it."
  [this] {:pre [(parser? this)]}
  (fn [pos inp cont]
    (this pos inp
          (fn [npos inner]
            (inner npos inp cont)))))

(defn $
  "Maps parser's values. With single argument it equals to `return`. With 2 and
   more arguments the first is a function to apply to parsers' result values 
   and the rest are the parsers. Number of parsers arguments should be equal
   to function's arity."
  ([v] (return v))
  ([f i] {:pre [(fn? f) (parser? i)]}
    (fn [pos inp cont]
      (i pos inp
         (fn [npos v] 
           (cont npos (f v))))))
  ([f i & others] {:pre [(fn? f) (parser? i) (every? parser? others)]}
    ($ (partial apply f) (seq (cons i others)))))

(defmacro $->
  "Maps parser's value through the forms using `->`."
  [this & forms]
  `($ (fn [k#] (-> k# ~@forms)) ~this))

(defmacro $->>
  "Maps parser's value through the forms using `->>`."
  [this & forms]
  `($ (fn [k#] (->> k# ~@forms)) ~this))

(defn >>=
  "Monadish bind. It is better to avoid it because it is quite hard to reason
   for ambiguous grammars."
  [arg fun] 
  (join ($ fun arg)))

(def ^:private memoId (atom 0))
(def ^:dynamic ^:private *memoTable*)

(defn- getMemoTable [id] 
  (let [mt @*memoTable*]
    (or (mt id) 
        (let [r (atom {})] 
          (reset! *memoTable* (assoc mt id r)) r))))

(defn memo 
  "Memoises the parser."
  [this] {:pre [(parser? this)]}
  (let [id (swap! memoId inc)]
    (fn [pos inp cont]
      (let [table (getMemoTable id)]
        (if-let [[vals conts] (@table pos)]
          (do
            (swap! conts conj cont)
            (doseq [[npos nval] @vals] (cont npos nval)))
          (let [vals (atom #{})
                conts (atom [cont])]
          (swap! table assoc pos [vals conts])
          (this pos inp 
                (fn [npos nval]
                  (when-not (contains? @vals nval) 
                    (swap! vals conj [npos nval])
                    (doseq [i @conts] (i npos nval)))))))))))

(defn run
  "Runs the parser for `input`. Returns vector of possible parser's values.
   It is only 1 value if the parser is non ambiguous."
  [this input]
  (let [res (transient [])]
    (binding [*memoTable* (atom {})]
      (this 0 (vec input)
            (fn [npos val]
              (conj! res val))))
    (persistent! res)))

(defn- deferRef [ref]
  (fn [pos inp cont] (@ref pos inp cont)))  

(defmacro defer 
  "Defers parser creation for recursive grammar nodes."
  [opt]
  `(fn [pos# inp# cont#]
     (~opt pos# inp# cont#)))

(defn fix
  "Helper combinator for recursive grammars creation. Calls f with an argument 
   refereeing to parser node is to be return by the function and returns that 
   parser."
  [f] {:pre (fn? f)}
  (let [r (atom nil)
        s (f (trace "rec" (memo (deferRef r))))]
    (reset! r s)))

(def ^:private kwVal {})

(defn full
  [this] ($ first (seq this eof)))

(defn many
  "applies the `this` parser non-deterministcaly [0, 1, ...] times
   returns collection of results"
  [this] {:pre [(parser? this)]} 
  (fix #(alt (return []) ($ cons this %))))

(defn many!
  "eager determenistic version of many"
  [this] {:pre [(parser? this)]} 
  (fix #(once (alt ($ cons this %) (return [])))))

(defn run1 [parser input] 
  (let [n (run parser input)]
    (cond
      (empty? n) (throw (IllegalArgumentException. (format "no parse for %s" input)))
      (= (count n) 1) (first n)
      :else (throw (IllegalArgumentException.
                     (format "%s is ambiguous, options are: %s" input n))))))

(defn exp-table 
  "Builds mixfix expressions parser."
  [table factor] {:pre [(map? table), (not (empty? table))]}
    (let [
          st (into (sorted-map) table)
          m (into (sorted-map) (for [[i] table] [i (atom zero)]))
          top (->> m first val deferRef (trace "top") memo)
          res (full top)
          f (memo factor)
          byprec (fn [k] 
                   (let [v (first (filter #(>= (key %) k) m))]
                    (if v (deferRef (val v)) f)))
          prim (fn [t]
                 (if (number? t) 
                   (byprec t)
                   ($ (constantly kwVal) (sym t))))
          layers (into (hash-map) 
                       (reductions
                         (fn [[ai av] [ci cv]] [ci (alt av cv)])
                         [0 f]
                         (for [[i v] (rseq st)]
                           [i (apply alt
                                     (for [[pat fun] v]
                                       ($
                                         (fn [v]
                                           (apply fun (filter
                                                        #(not (identical? kwVal %)) v)))
                                         (seq (map prim pat)))))])))]
      (doseq [[k ref] m]
        (reset! ref (memo (trace (format "layer:%d" k) (layers k)))))
      res))


