(ns itmo-ak-lab4.parser)

(defn tokenize
  "Splits the source text into a list of tokens."
  [text]
  (re-seq #"[()]|\"[^\"]*\"|[^\s()\"]+" text))

(comment
  (tokenize "(+ 1 2)")
  (count (tokenize "(print-str \"Hello, world!\")"))
  (count (tokenize "(if (= (mod n 2) 0) (/ n 2) (+ 1 (* 3 n)))")))
