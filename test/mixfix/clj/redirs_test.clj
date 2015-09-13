(ns mixfix.clj.redirs_test
  (:require [clojure.test :refer :all]
            [mixfix.clj :as r]
            ))

(r/op 400 + [[assoc id 0] + [+]])

(r/defn t1 [x y z] (x + y + z))

(deftest simple []
  (is (= (r/% 2 + (2 + 2)) 6))
  (is (= (t1 2 3 4) 9)))

(macroexpand '(r/% 0 + 1))
