(ns mixfix.clj.core
  (:require [mixfix.clj.parser :as r]
            [clojure.walk :as w]
            cljs.analyzer))

(defmacro declare-lang 
  "Defines a name which may be used to specify scopes of mixfix operators.
   The second optional arguments specifies which language to copy initial 
   definitions from. The definitions are copied only once."
  ([nm] `(def ~nm (atom {})))
  ([nm par] `(def ~nm (atom @~par))))

(declare-lang global)

(def ^:dynamic *clojure-dialect* "Clojure dialect name" :clj)

(def ^:dynamic *lang* 
  "specifies current language"
  global)

(defn- hole-opts [opts] 
  (->> opts (partition 2 1) (map vec) (into {})))

(defn- picture-arity [picture]
  (count (filter number? picture)))

(defn- lang-def? [d] (and (:read d) (:write d)))

(defn- compile-hole [prec opts] 
  (let [[pre [_ ids & post]] (split-with #(not= :id (keyword %)) opts)
        opts (->> (concat pre post) (map (some-fn keyword identity)) set)
        [pname & nprec] (filter (some-fn #{:+} number?) opts)
        _ (when-not (empty? nprec)
            (throw (IllegalArgumentException. 
                     (format "only single precidence is allowed in %s" opts))))
        prec (cond
               (number? pname) pname
               (nil? pname) prec
               :else (inc prec))
        asc (contains? opts :assoc)
        rst (vec (remove (some-fn #{:+ :assoc} number?) opts))]
    (when-not (empty? rst)
      (throw (IllegalArgumentException. 
                     (format "unknown items in syntax hole definition %s" rst))))
    [prec asc ids]))

(defn- compile-picture
  [prec picture] {:pre [(number? prec)]}
  (let [nprec (inc prec)
        asc (volatile! nil)
        tpict (for [i picture] (if (coll? i) (compile-hole prec i) i))
        rpict (for [i tpict] (if (coll? i) (first i) i))
        [asc & ascr] (->> tpict (filter coll?)
                       (keep-indexed #(if (second %2) [%1 (last %2)])))
        _ (when-not (empty? ascr) 
            (throw (IllegalArgumentException. 
                                 (format 
                                   "only a single assoc option is allowed %s"
                                   picture))))]
    [(vec rpict) asc]))

(defn- print-group-pict [xf]
  (let [buf (volatile! [])] 
  (fn
    ([] (xf))
    ([result] (xf [result @buf]))
    ([result input] 
      (if (number? input)
        (let [cur @buf]
          (vreset! buf [])
          (xf result [cur input]))
        (do
          (vswap! buf conj input)
          result))))))

(defn- print-compile-pict [pict] {:pre [(not (empty? pict))]}
  (transduce print-group-pict conj pict))

(def ^:dynamic ^:private *prec* 0)

(defn- add-parens [prec col]
  (if (< prec *prec*) [(apply list col)]
    (vec col)))

(defn print-from-table
  "Converts plain clojure form into a form with mixfix operators."
  [table col] {:pre [(coll? col)]}
  (if (<= (count col) 1) 
    [col]
    (let [[head & args] col
          arity (count args)
          outarg (fn [arg [pfx aprec]] 
                   (if (seq? arg)
                     (concat pfx 
                             (binding [*prec* aprec]
                               (print-from-table table arg)))
                     (conj pfx arg)))
          iter (fn [prec args pict tail]
                 (add-parens prec (concat (mapcat outarg args pict) tail)))]
          (if-let [[prec [pict tail]] 
                   (get-in table [head arity])]
            (iter prec args pict tail)
            (if-let [[prec [pict tail oparity pos empv]] 
                     (get-in table [head '*])]
              (let [[pre nxt] (split-at pos args)
                    len (- arity oparity -1)]
                (if (neg? len) col
                  (let [[inner post] (split-at len nxt)
                        inner (cond
                                (zero? len) empv
                                (== len 1) (first inner)
                                :else (list* head inner))
                        args (concat pre [inner] post)]
                    (iter prec args pict tail))))
              col)))))

(defn- get-assoc [picture]
  (let [[[x [_ _ opt]] & rst] 
        (filter #(some-> % second second name #{"assoc"}) 
                (map-indexed list (filter vector? picture)))]
    (when rst
      (throw (IllegalArgumentException. "only one assoc spec is allowed")))
    (if x [x opt])))

(defn- do-add-op [table prec symbol pict]
  (let [[cpict asc] (compile-picture prec pict)
        arity (picture-arity cpict)
        ppict (print-compile-pict cpict)
        fun (if-let [[ix iden] asc]
              (fn [& args]
                (let [[pre [cur & post]] (split-at ix args)]
                  (cons symbol 
                        (if (= iden cur) 
                          (concat pre post)
                          (if (and (coll? cur) (= (first cur) symbol)) 
                            (concat pre (rest cur) post)
                            args)))))
              (fn [& args] (cons symbol args)))
        [arity popts] (if-let [[ix iden] asc]
                        ['* (concat ppict [arity ix iden])]
                        [arity ppict])]
    (-> table
      (update-in [:read prec] (partial apply assoc) [cpict fun])
      (assoc-in [:write symbol arity] [prec popts])
      (update-in [:origin] conj [prec symbol pict])
      (assoc-in [:clj] nil)
      (assoc-in [:cljs] nil))))

(defn- add-ops [table ops]
  (reduce (fn [table op] (apply (partial do-add-op table) op)) table ops))

(defn- del-op [table symbol]
  (add-ops {} (remove #(= (name symbol) (name (second %))) (:origin table))))

(defn add-op [prec picture symbol]
  (swap! *lang* do-add-op prec picture symbol))

(defn rm-op [symbol] (swap! *lang* del-op symbol))

(defmacro op
  "Defines mixfix operator. First optional argument is a name for the 
   operator's scope. The second is precedence level of the operator. The bigger 
   the number the tightly the operator binds. The third is resulting form head
   symbol. And the last one is a mixfix picture of the operator."
  ([lang prec symbol picture]
    (binding [*lang* @(resolve &env lang)] (add-op prec symbol picture) nil))
  ([prec symbol picture] (add-op prec symbol picture) nil))

(defmacro remove-op
  "removes all operators which output forms with this head symbol"
  ([lang symbol] (binding [*lang* @(resolve &env lang)] 
                   (rm-op symbol) nil))
  ([symbol] (rm-op symbol) nil))

(def ^:dynamic *locals*
  "Specifies locally defined symbols, 
   so they are not considered to be syntax part."
  #{})

(def ^:dynamic ^:private *keywords* #{})

(def ^:private specials (atom #{}))

(defmacro reg-sym
  "Adds a symbol to a list of known reserved words. For which are not local or
   cannot be resolved (some other library EDSL symbols)."
  [n] (swap! specials conj n) 'nil)

(reg-sym do)

(defn- check-locals [n] (*locals* n))

(defn- prim?-def [] (every-pred
                      (complement *keywords*)
                      (some-fn (complement symbol?)
                               check-locals
                               (case *clojure-dialect*
                                 :cljs (partial cljs.analyzer/resolve-var nil)
                                 :clj (partial resolve)))))

(def ^:dynamic prim? prim?-def)
  
(defn prim [] (r/guard r/any (prim?)))
(defn clj-mixfix
   "builds mixfix expression table with clojure syntax"
   [table]
   (binding
     [*keywords* (set (for [[_ i] table [j _] i k j :when (symbol? k)] k))]
     (r/exp-table table (prim))))

(defn describe-lang 
  ([] (:origin @*lang*))
  ([lang] (:origin @lang)))

(defn- get-parser []
  (let [ldef @*lang*]
    (or (*clojure-dialect* ldef)
        (let [res (clj-mixfix (:read ldef))]
          (swap! *lang* assoc *clojure-dialect* res)
          res))))

(def ^:dynamic *clojure-apps* 
  "Defines if the library should try to fallback to plain clojure forms parsing" 
  true)

(defn parse
  "Parses 1 level syntax with mixfix operators. Returns plain clojure form
   without them if succeed."
  [col]
  (let [prim-check (prim?)]
    (if (< (count col) 2)
      (apply list col)
      (let [parser (get-parser)
            r (r/run parser col)]
        (cond 
          (empty? r) (if (and *clojure-apps* (every? prim-check col))
                       col
                       ;'(str (format "no parse for: %s" col))
                       (throw (IllegalArgumentException. 
                               (format "no parse for: %s" col))))
          (= 1 (count r)) (first r)
          :else (throw (IllegalArgumentException. 
                         (format "%s is ambiguous, options are: %s" col r))))))))

(defn parse-all
  "Deep version of `parse`. Parses also inside sub-forms."
  [col] 
  (w/prewalk #(if (list? %) (parse %) %) col))

(defmacro %1
  "Shallow version of %."
  [& col]
  (let [f (binding [*locals* (into @specials (keys &env))
                    *clojure-dialect* (if (:ns &env) :cljs :clj)]
            (parse col))]
    `(~@f)))

(defn propagate-%
  ([v] 
    (w/prewalk (fn [i] (if (list? i) (cons (var %1) i) i)) v))
  ([lang v] (w/prewalk (fn [i] (if (list? i) (list* '%1* lang i) i)) v)))

(defmacro %
  "Transforms form and its subforms into clojure syntax without mixfix ops and
   evaluate it."
  [& f]
  `(%1 ~@(propagate-% (vec f))))

;;; specifying custom language
(defmacro %1*
  "Shallow version of %*."
  [lang & col]
  (let [f (binding [*locals* (into @specials (keys &env))
                    *lang* @(resolve lang)
                    *clojure-dialect* (if (:ns &env) :cljs :clj)]
            (parse col))]
    `(~@f)))

(defmacro %*
  "Transforms form and its subforms into clojure syntax without mixfix ops. 
   First argument specifies language name."
  [lang & f]
  `(%1* ~lang ~@(propagate-% lang (vec f))))

(defn to-mixfix-1
  "Reverse of %. Takes a plain clojure form and transforms it to the one with
   mixifix operators."
  [col] (->> col (print-from-table (:write @*lang*)) (apply list)))

(defn to-mixfix [col] (w/prewalk 
                        #(if (and (list? %) (> (count %) 1)) 
                             (to-mixfix-1 %) %) 
                        col))


(defmacro form [& args] `(~@args))

(op 1000 mixfix.clj.core/form [[assoc] [+]])

nil

