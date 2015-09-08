(ns clojure.tools.mixfix.sql-test
  (:refer-clojure :exclude [*])
  (:require [clojure.test :refer :all]
            [clojure.tools.mixfix.core :as r]
            ))

(r/declare-lang sql)
(r/op sql 100 select [select [+] from [+] where [+] group by [+]]) 
(r/op sql 100 select [select [+] from [+] where [+]]) 
(r/op sql 100 select [select [+] from [+]]) 
(r/op sql 200 list [[assoc] [+]])
(r/op sql 150 = [[+] = [+]])
(r/op sql 150 < [[+] < [+]])

(defmacro check [c a]
  `(do
     (is (= '(~@a) (r/parse '(~@c))))
     (is (= '(~@c) (r/to-mixfix '(~@a))))))

(deftest test-sql []
  (binding [r/*lang* sql
            r/*locals* #{'* 'col1 'col2 'table1 'table2}]
    (check (select * from table1)
           (select * table1))
    (check (select col1, col2 from table1, table2 where col1 = col2)
           (select (list col1 col2) (list table1 table2) (= col1 col2)))
    (check (select * from table1, table2 where col1 < col2 group by col1, col2)
           (select * (list table1 table2) (< col1 col2) (list col1 col2)))))
