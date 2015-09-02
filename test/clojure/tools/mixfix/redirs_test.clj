(ns clojure.tools.mixfix.redirs_test
  (:require [clojure.test :refer :all]
            [clojure.tools.mixfix :as r]
            ))

(r/op 400 + [[x] + [X]])

(r/defn t1 [x y z] (x + y + z))

(deftest simple []
 (is (= (r/% 2 + (2 + 2)) 6))
 (is (= (t1 2 3 4) 9)))

