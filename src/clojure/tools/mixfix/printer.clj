(ns clojure.tools.mixfix.printer
  "Utilities for adding mixfix operators into plain clojure forms.")

(defn- group-pict [xf]
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

(defn compile-pict [pict] {:pre [(not (empty? pict))]}
  (transduce group-pict conj pict))

(def ^:dynamic ^:private *prec* 0)

(defn- add-parens [prec col]
  (if (< prec *prec*) [(apply list col)]
    (vec col)))

(defn from-table
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
                               (vec (from-table table arg))))
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

nil
