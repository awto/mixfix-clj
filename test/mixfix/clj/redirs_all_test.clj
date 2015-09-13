(ns mixfix.clj.redirs-all-test
  (:refer-clojure :exclude [defn def])
  (:require [clojure.test :refer :all]
            [mixfix.clj :refer :all]))

(op 400 + [[assoc] + [+]])

(defn t1 [x y z] (x + y + z))

(deftest simple []
  (is (= (% 2 + (2 + 2)) 6))
  (is (= (t1 2 3 4) 9)))
