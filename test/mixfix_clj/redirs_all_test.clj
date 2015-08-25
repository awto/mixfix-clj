(ns mixfix-clj.redirs-all-test
  (:refer-clojure :exclude [defn def])
  (:require [clojure.test :refer :all]
            [mixfix-clj.redirs :refer :all]))

(op 400 + [[x] + [X]])

(defn t1 [x y z] (x + y + z))

(deftest simple []
  (is (= (% 2 + (2 + 2)) 6))
  (is (= (t1 2 3 4) 9)))

(run-tests)


