(ns itmo-ak-lab4.isa)

(def registers
  "register name -> 4-bit number"
  {:zero 0
   :t0 1 :t1 2 :t2 3 :t3 4 :t4 5 :t5 6 :t6 7 :t7 8 :t8 9 :t9 10 :t10 11 :t11 12
   :fp 13
   :sp 14
   :ra 15})

(def instructions
  "Mnemonic -> opcode (8 bits) and format"
  {:halt {:opcode 0x00 :format :none}    ; 0000 0000
   :nop {:opcode 0x01 :format :none}     ; 0000 0001
   :add {:opcode 0x02 :format :r}        ; 0000 0010
   :sub {:opcode 0x03 :format :r}        ; 0000 0011
   :mul {:opcode 0x04 :format :r}        ; 0000 0100
   :div {:opcode 0x05 :format :r}        ; 0000 0101
   :mod {:opcode 0x06 :format :r}        ; 0000 0110
   :slt {:opcode 0x07 :format :r}        ; 0000 0111
   :sltu {:opcode 0x08 :format :r}       ; 0000 1000
   :seq {:opcode 0x09 :format :r}        ; 0000 1001
   :addi {:opcode 0x10 :format :i}       ; 0001 0000
   :lui {:opcode 0x11 :format :i}        ; 0001 0001
   :ori {:opcode 0x12 :format :i}        ; 0001 0010
   :lw {:opcode 0x13 :format :i}         ; 0001 0011
   :sw {:opcode 0x14 :format :i}         ; 0001 0100
   :lb {:opcode 0x15 :format :i}         ; 0001 0101
   :sb {:opcode 0x16 :format :i}         ; 0001 0110
   :beq {:opcode 0x20 :format :i}        ; 0010 0000
   :bne {:opcode 0x21 :format :i}        ; 0010 0001
   :jmp {:opcode 0x22 :format :i}        ; 0010 0010
   :jal {:opcode 0x23 :format :i}        ; 0010 0011
   :jr {:opcode 0x24 :format :r}})       ; 0010 0100

(defn move-type-r [{:keys [op a b c]}]
  (let [opcode (get-in instructions [op :opcode])
        reg-a  (get registers a 0)
        reg-b  (get registers b 0)
        reg-c  (get registers c 0)]
    (bit-or (bit-shift-left opcode 24)
            (bit-shift-left reg-a  20)
            (bit-shift-left reg-b  16)
            (bit-shift-left reg-c  12))))

(defn move-type-imm [{:keys [op a b imm]}]
  (let [op (get-in instructions [op :opcode])
        reg-a (get registers a 0)
        reg-b (get registers b 0)]
    (bit-or (bit-shift-left op 24)
            (bit-shift-left reg-a 20)
            (bit-shift-left reg-b 16)
            (bit-and imm 0xFFFF))))

(defn move-type-none [op]
  (bit-shift-left (get-in instructions [op :opcode]) 24))

(defn encode
  "instruction map -> 32-bit word"
  [instruc]
  (let [op (:op instruc)]
    (case (get-in instructions [op :format])
      :none (move-type-none op)
      :r (move-type-r instruc)
      :i (move-type-imm instruc))))

(defn to-signed-16
  "0..65535 -> -32768..32767 (two's complement)."
  [n]
  (if (>= n 0x8000)
    (- n 0x10000)
    n))

(def opcode->op
  "Opcode number -> mnemonic."
  (into {} (for [[k v] instructions] [(:opcode v) k])))

(def number->reg
  "Reg number -> register name"
  (into {} (for [[k v] registers] [v k])))

(defn decode
  "32-bit word -> instruction map."
  [word]
  (let [opcode (opcode->op (bit-and (bit-shift-right word 24) 0xFF))
        a (number->reg (bit-and (bit-shift-right word 20) 0xF))
        b (number->reg (bit-and (bit-shift-right word 16) 0xF))
        c (number->reg (bit-and (bit-shift-right word 12) 0xF))
        imm (to-signed-16 (bit-and word 0xffff))]
    (case (get-in instructions [opcode :format])
      :none {:op opcode}
      :r {:op opcode :a a :b b :c c}
      :i {:op opcode :a a :b b :imm imm})))

