(ns itmo-ak-lab4.parser-test
  (:require [clojure.test :refer [deftest is testing]]
            [itmo-ak-lab4.parser :as p]))

(deftest tokenize-test
  (testing "a string with spaces is one token"
    (is (= 4 (count (p/tokenize "(print-str \"Hello, world!\")")))))
  (testing "parentheses, names and numbers are separate tokens"
    (is (= ["(" "+" "1" "2" ")"]
           (p/tokenize "(+ 1 2)")))))

(deftest parse-atom-test
  (testing "number"
    (is (= 42 (p/parse-atom "42"))))
  (testing "string without quotes"
    (is (= "hi" (p/parse-atom "\"hi\""))))
  (testing "name becomes a symbol"
    (is (= 'mod (p/parse-atom "mod")))))

(deftest read-form-test
  (testing "nested lists"
    (is (= '(+ 1 (* 2 3))
           (first (p/read-form (p/tokenize "(+ 1 (* 2 3))"))))))
  (testing "returns the tokens that were not read"
    (is (= ["x"]
           (second (p/read-form (p/tokenize "(+ 1 2) x")))))))

(deftest parse-test
  (testing "collatz-next is one defun form"
    (let [forms (p/parse "(defun collatz-next (n)
                            (if (= (mod n 2) 0) (/ n 2) (+ 1 (* 3 n))))")]
      (is (= 1 (count forms)))
      (is (= 'defun (first (first forms))))))
  (testing "hello has two top-level forms"
    (let [forms (p/parse "(defun print-str (s) (print-char s))
                          (print-str \"Hello, world!\")")]
      (is (= 2 (count forms)))
      (is (= '(print-str "Hello, world!") (second forms))))))

(deftest error-test
  (testing "a ) without its ( is an error"
    (is (thrown? clojure.lang.ExceptionInfo (p/parse ")")))))