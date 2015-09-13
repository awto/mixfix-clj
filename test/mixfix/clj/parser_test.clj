(ns mixfix.clj.parser-test
  (:require [mixfix.clj.parser :refer :all]
            [clojure.test :refer :all])
  (:refer-clojure :exclude [list seq]))

(def N (alt (tok :student) (tok :professor)))
(def Det (alt (tok :every) (tok :no)))
(def V (alt (tok :likes) (tok :knows)))
(def PN (alt (tok :Kim) (tok :Sandy)))
(def NP (alt PN (seq Det N)))
(declare VP)
(def S (defer (seq NP VP)))
(def VP (alt (seq V NP) (seq V S)))


(def NP2 (memo (defer (alt PN (seq NP2 N) (seq Det N)))))
(declare VP2)
(def S2 (defer (seq NP2 VP2)))
(def VP2 (alt (seq V NP2) (seq V S2)))

(deftest test-parser-simple []
  (is (= (run S [:Kim :knows :every :student :likes :Sandy])
        ['(:Kim (:knows (:every :student))) 
         '(:Kim (:knows ((:every :student) (:likes :Sandy))))]
      ))
  (is (= (run S2 [:Kim :professor :knows :every :student :likes :Sandy])
         ['((:Kim :professor) (:knows (:every :student))) 
          '((:Kim :professor) (:knows ((:every :student) (:likes :Sandy))))]
         ))
  (is (= (run S2 [:Kim :professor :knows :every :student])
         ['((:Kim :professor) (:knows (:every :student)))]))
  (is (= (run1 ($-> (return 100) inc (- 100)) []) 1))
  (is (= (run1 ($->> (return 100) inc (- 100)) []) -1))
  (is (= (run1 (>>= (return 1) #(return (inc %))) [])) 2)
  (is (= (run (>>= (return 1) (constantly zero)) []) []))
  (is (= (run1 (once (alt (return 1) 
                          ($ #(throw (Exception. (str %))) (return 2)))) []) 1))
  (is (= (run1 (many! (tok 1)) [1 1 1 1]) [1 1 1 1]))
  (is (= (run (many (tok 1)) [1 1 1 1]) [[] [1] [1 1] [1 1 1] [1 1 1 1]])))
