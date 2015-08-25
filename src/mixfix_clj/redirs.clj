(ns mixfix-clj.redirs
   (:refer-clojure :exclude [defn])
   (:require [clojure.core :as c] [mixfix-clj.core :as %]))

(defmacro defn
  "Same as clojure.core/defn but unwraps mixfix operators"
  [& ops]
  `(c/defn ~@(%/propagate-% ops)))

(defmacro op [& args] `(%/op ~@args))
(defmacro % [& args] `(%/% ~@args))
(defmacro %* [& args] `(%/%* ~@args))

nil
