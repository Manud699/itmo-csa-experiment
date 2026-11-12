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
  {:halt {:opcode 0x00 :format :none}     ; 00 0000
   :nop {:opcode 0x01 :format :none}      ; 00 0001
   :add {:opcode 0x02 :format :r}         ; 00 0010
   :sub {:opcode 0x03 :format :r}         ; 00 0011
   :mul {:opcode 0x04 :format :r}         ; 00 0100
   :div {:opcode 0x05 :format :r}         ; 00 0101
   :mod {:opcode 0x06 :format :r}         ; 00 0110
   :slt {:opcode 0x07 :format :r}         ; 00 0111
   :sltu {:opcode 0x08 :format :r}        ; 00 1000
   :seq {:opcode 0x09 :format :r}         ; 00 1001
   :addi {:opcode 0x10 :format :i}        ; 01 0000
   :lui {:opcode 0x11 :format :i}         ; 01 0001
   :ori {:opcode 0x12 :format :i}         ; 01 0010
   :lw {:opcode 0x13 :format :i}          ; 01 0011
   :sw {:opcode 0x14 :format :i}          ; 01 0100
   :lb {:opcode 0x15 :format :i}          ; 01 0101
   :sb {:opcode 0x16 :format :i}          ; 01 0110
   :beq {:opcode 0x20 :format :i}         ; 10 0000
   :bne {:opcode 0x21 :format :i}         ; 10 0001
   :jmp {:opcode 0x22 :format :i}         ; 10 0010
   :jal {:opcode 0x23 :format :i}         ; 10 0011
   :jr {:opcode 0x24 :format :r}})        ; 10 0100

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
