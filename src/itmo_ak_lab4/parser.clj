(ns itmo-ak-lab4.parser)

(defn tokenize
  "Splits the source text into a list of tokens."
  [text]
  (re-seq #"[()]|\"[^\"]*\"|[^\s()\"]+" text))

(defn parse-atom
  "Turns one token into a number, a string or a symbol."
  [token]
  (cond
    (parse-long token) (parse-long token)
    (.startsWith token "\"") (subs token 1 (dec (count token)))
    :else (symbol token)))

(comment
  (tokenize "(+ 1 2)")
  (count (tokenize "(print-str \"Hello, world!\")"))
  (count (tokenize "(if (= (mod n 2) 0) (/ n 2) (+ 1 (* 3 n)))"))
  (parse-atom "\"hola\"")
  (map parse-atom (tokenize "(+ 1 2)")))
