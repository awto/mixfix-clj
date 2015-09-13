(ns clojure.tools.mixfix.assoc_test
  (:require [clojure.test :refer :all]
            [clojure.tools.mixfix.core :refer :all]))

(op 2000 oper1 [[+] $$ [assoc]])
(op 2000 oper2 [[assoc id 0] || [+]])

(defmacro check [c m] 
   `(do
      (is (= (parse-all '(~@m)) '(~@c)))
      (is (= (to-mixfix '(~@c)) '(~@m)))))

(deftest rassoc-operators []
  (check (oper1 1 2) (1 $$ 2))
  (check (oper1 1 2 3) (1 $$ 2 $$ 3))
  (check (oper1 1 2 3 4) (1 $$ 2 $$ 3 $$ 4))
  (check (oper1 (oper1 1 2) 3 4) ((1 $$ 2) $$ 3 $$ 4))
  (check (oper1 (oper1 1 2 3) 4 5) ((1 $$ 2 $$ 3) $$ 4 $$ 5))
  (check (oper1 1 (oper1 2 3) 4) (1 $$ (2 $$ 3) $$ 4))
  (is (= (to-mixfix '(oper1 1 2 (oper1 3 4))) '(1 $$ 2 $$ 3 $$ 4)))   
  (is (= (to-mixfix '(oper1 1 (oper1 2 (oper1 3 4)))) '(1 $$ 2 $$ 3 $$ 4)))   
  (check (oper1 (oper1 (oper1 (oper1 1 2) 3) 4) 5) ((((1 $$ 2) $$ 3) $$ 4) $$ 5)))

(deftest lassoc-operators []
  (check (oper2 1) (0 || 1))
  (check (oper2 1 0) (1 || 0))
  (check (oper2 1 2) (1 || 2))
  (check (oper2 1 2 3) (1 || 2 || 3))
  (check (oper2 1 (oper2 2 3)) (1 || (2 || 3)))
  (check (oper2 1 (oper2 2 (oper2 3 4))) (1 || (2 || (3 || 4)))))

(deftest space-operators []
  (add-op 1000 'form [['assoc] ['+]])
  (add-op 200 'if ['if ['+] 'then []])
  (is (= (parse '(if = 2 2 then :t)) '(if (form = 2 2) :t)))
  (rm-op 'form)
  (is (= (parse '(if (= 2 2) then :t)) '(if (= 2 2) :t)))
  (add-op 1000 'list [['assoc] ['+]])
  (is (= (parse '(1 2 3)) '(list 1 2 3)))
  (rm-op 'list))
