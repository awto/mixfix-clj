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

(defn add-ops
  "Converts plain clojure form into a form with mixfix operators."
  [table col] {:pre [(coll? col)]}
  (if (<= (count col) 1) [col]
    (let [[head & args] col
          arity (count args)]
      (if-let [[prec [pict tail]] (get-in table [head arity])]
          (add-parens prec
            (concat 
              (mapcat
                (fn [arg [pfx aprec]]
                  (if (list? arg)
                    (concat pfx 
                            (binding [*prec* aprec] 
                              (vec (add-ops table arg))))
                    (conj pfx arg)))
                 args pict)
              tail))
        col))))
