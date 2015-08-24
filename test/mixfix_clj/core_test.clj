(ns mixfix-clj.core-test
  (:require [clojure.test :refer :all]
            [mixfix-clj.core :refer :all]
            [mixfix-clj.parser :as r]
            ))

(op 400 + [[x] + [X]])
(op 400 - [[x] - [X]])
(op 500 * [[x] * [X]])
(op 500 / [[x] / [X]])

(declare-lang bools global)

(op 200 or [[x] or [X]]) 
(op bools 200 or [[x] or [X]]) 
(op bools 300 and [[x] and [X]]) 
(op bools 400 = [[x] == [x]])

(op bools 100 when [if [X] then [x]])
(op bools 110 if [if [X] then [x] else [x]])

(def z 4)

(deftest simple-arith []
    (is (= (% 3 - 2 - 1) 0))
    (is (= (% 16 / 4 / 2 / 2) 1))
    (is (= (% 1 + 2 * 2 - 2 * 3)))
    (is (= (% (2 + 2) * 2) 8))
    (let [x 2 y 3]
      (is (= (% x + y * z) 14)))
    (is (= (% (+ 2 2) + 3) 7))
    (is (= (let [x 2] (% let [a (2 + 3) b (- a x)] (a - b))) 2)))

(deftest simple-bools []
  (%* bools do
    (is (= (true or false)) true)
    (is (= (2 + 3) 5))
    (is (= (if true then 2 else 3) 2))
    (is (= (if 2 == 3 then 2 else 3) 3))
    (is (= (if 2 == 2 then if 2 == 3 or 3 == 4 then 1 else 2 else 3) 2))))

(def ^:dynamic *cur-lang* global)

(defmacro check [c m] 
   `(do
      (is (= (parse-all '(~@m)) '(~@c)))
      (is (= (to-mixfix '(~@c)) '(~@m)))))

(deftest arith-print []
 (check (+ 2 3) (2 + 3))
 (check (+ (+ 2 3) 4) (2 + 3 + 4))
 (check (+ 2 (+ 3 4)) (2 + (3 + 4)))
 (check (+ (* 2 3) 4) (2 * 3 + 4))
 (check (+ 2 (* 3 4)) (2 + 3 * 4))
 (binding [*locals* #{'x 'y}]
   (check (+ (+ (+ x y) (rand)) 2) (x + y + (rand) + 2)))
 (is (thrown? IllegalArgumentException (parse '(x + 1)))))

(deftest bool-print []
  (binding [*lang* bools] 
    (check (or true false) (true or false))
    (check (and 1 (or 2 3)) (1 and (2 or 3)))
    (check (or 1 (and 2 3)) (1 or 2 and 3))
    (check (when (= 2 3) (if (= 4 3) (if (= 5 6) 11 10) 20))
           (if 2 == 3 then if 4 == 3 then if 5 == 6 then 11 else 10 else 20))
    (is (thrown? IllegalArgumentException 
               (binding [*lang* bools] (parse '(if 2 = 3 then 2 else 3)))))))

