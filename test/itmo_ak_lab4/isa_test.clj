(ns itmo-ak-lab4.isa-test
  (:require [clojure.test :refer [deftest is testing]]
            [itmo-ak-lab4.isa :as isa]))

(deftest opcodes-test
  (testing "every instruction has its own opcode"
    (is (= 22 (count isa/instructions)))
    (is (= 22 (count (distinct (map :opcode (vals isa/instructions))))))))

(deftest encode-test
  (testing "format :none"
    (is (= 0x00000000 (isa/encode {:op :halt})))
    (is (= 0x01000000 (isa/encode {:op :nop}))))
  (testing "format :r"
    (is (= 0x02123000 (isa/encode {:op :add :a :t0 :b :t1 :c :t2})))
    (is (= 0x24F00000 (isa/encode {:op :jr :a :ra}))))
  (testing "format :i"
    (is (= 0x1011FFFF (isa/encode {:op :addi :a :t0 :b :t0 :imm -1})))
    (is (= 0x22000005 (isa/encode {:op :jmp :imm 5})))))

(deftest decode-test
  (is (= {:op :add :a :t0 :b :t1 :c :t2} (isa/decode 0x02123000)))
  (is (= {:op :addi :a :t0 :b :t0 :imm -1} (isa/decode 0x1011FFFF)))
  (is (= {:op :nop} (isa/decode 0x01000000))))

(deftest to-signed-16-test
  (is (= 5 (isa/to-signed-16 0x0005)))
  (is (= 32767 (isa/to-signed-16 0x7FFF)))
  (is (= -32768 (isa/to-signed-16 0x8000)))
  (is (= -1 (isa/to-signed-16 0xFFFF))))

(deftest round-trip-test
  (testing "encode -> decode -> encode gives the same word"
    (doseq [instr [{:op :add :a :t0 :b :t1 :c :t2}
                   {:op :jr :a :ra}
                   {:op :lw :a :t1 :b :sp :imm -32768}
                   {:op :beq :a :t0 :b :zero :imm 32767}
                   {:op :halt}]]
      (let [word (isa/encode instr)]
        (is (= word (isa/encode (isa/decode word))))))))
