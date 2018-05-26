(ns chip8.core
  (:require [clojure.core.match :refer [match]]
            [quil.core :as q])
  (:gen-class))

;; ------------------------------------------------------------------------------------------------------
;; ------------------------------------ GLOBALS AND HELPER FUNCTIONS ------------------------------------
;; ------------------------------------------------------------------------------------------------------

(defn file-to-bytes
  "Takes a string that holds a path to a file and
  returns the file as a vector of bytes."
  [file-path]
  (let [f (java.io.File. file-path)
        ary (byte-array (.length f))
        is (java.io.FileInputStream. f)]
    (.read is ary)
    (.close is)
    (into [] ary)))

(defn int-to-hex-str
  "Converts an integer to a hex string."
  [some-int]
  (format "%02x" some-int))

(defn hex-to-int
  "Takes a string which contains a hexadecimal value and converts it to an int."
  [hex-string]
  (Integer/parseInt hex-string 16))

(def fonts
  [0xF0 0x90 0x90 0x90 0xF0   ; 0
   0x20 0x60 0x20 0x20 0x70   ; 1
   0xF0 0x10 0xF0 0x80 0xF0   ; 2
   0xF0 0x10 0xF0 0x10 0xF0   ; 3
   0x90 0x90 0xF0 0x10 0x10   ; 4
   0xF0 0x80 0xF0 0x10 0xF0   ; 5
   0xF0 0x80 0xF0 0x90 0xF0   ; 6
   0xF0 0x10 0x20 0x40 0x40   ; 7
   0xF0 0x90 0xF0 0x90 0xF0   ; 8
   0xF0 0x90 0xF0 0x10 0xF0   ; 9
   0xF0 0x90 0xF0 0x90 0x90   ; A
   0xE0 0x90 0xE0 0x90 0xE0   ; B
   0xF0 0x80 0x80 0x80 0xF0   ; C
   0xE0 0x90 0x90 0x90 0xE0   ; D
   0xF0 0x80 0xF0 0x80 0xF0   ; E
   0xF0 0x80 0xF0 0x80 0x80]) ; F

(defn- make-memory
  "Create CHIP8 memory, which is 4096 bytes (4kb)."
  []
  (let [size 4096]
    (vec
      (take size
            (into fonts ;; Place the fonts in the beginning of memory.
                  (vec (repeat size 0)))))))

;; ## Screen setup
;;
;; The original implementation of the Chip-8 language used a
;; 64x32-pixel monochrome display with this format:
;;
;;        +----------------------+
;;        | (0,0)       (63,0)   |
;;        | (0,31)      (63,31)  |
;;        +----------------------+
;;

(defn make-screen
  "Create the hashmap that represents the screen."
  []
  (let [rows    32
        columns 64]
    {:rows rows
     :columns columns
     :memory (vec (repeat columns
                          (vec (repeat rows 0))))}))

(defn- get-in-range
  "Get arr from start to end.
  Ex: (get-in-range [0 1 2 3 4 5] 1 4) => [1 2 3]"
  ([arr start end] (get-in-range arr (range start end)))
  ([arr rge]
   (reduce #(conj %1 (get-in arr [%2])) [] rge)))

(defn- assoc-in-range
  "Update arr with val from start.
  Examples:
  (assoc-in-range [1 2 3 4 5] [6 7 8] 0) => [6 7 8 4 5]
  (assoc-in-range [1 2 3 4 5] [6 7 8] 2) => [1 2 6 7 8]"
  ([arr val] (assoc-in-range arr val 0))
  ([arr val start-or-range]
   (let [bound (range (count val))
         start start-or-range]
     (reduce #(assoc-in %1 [(+ %2 start)] (nth val %2)) arr bound))))

(defn ->bcd
  "Convert val to BCD (Binary Coded Decimal) array.
  Ex: (->bcd 123) => [1 2 3]."
  [val]
  [(int (/ val 100))
   (int (/ (mod val 100) 10))
   (int (mod val 10))])

(defn VxVy
  "Get the values of two V registers as a convenient vector."
  [V X Y]
  [(nth V X) (nth V Y)])

(defn read-register
  [state key]
  (key state))

(defn write-pc
  [{:keys [pc] :as state} val]
  (assoc-in state [:pc] val))

(defn add-to-pc
  [{:keys [pc] :as state} val]
  (write-pc state (+ pc val)))

(defn write-sp
  [state sp]
  (assoc-in state [:sp] sp))

(defn write-i
  [state i]
  (assoc-in state [:i] i))

(defn write-delay-timer
  [state dt]
  (assoc-in state [:delay-timer] dt))

(defn write-screen
  [state screen]
  (assoc-in state [:screen] screen))

(defn write-sound-timer
  [state st]
  (assoc-in state [:sound-timer] st))

(defn write-register
  [state key val]
  (assoc-in state [key] val))

(defn write-stack
  ([{:keys [stack] :as state} idx val]
   (write-stack state (assoc stack idx val)))
  ([state stack]
   (assoc-in state [:stack] stack)))

(defn write-memory
  ([{:keys [memory] :as state} arr start]
   (write-memory state (assoc-in-range memory arr start)))
  ([state memory]
   (assoc-in state [:memory] memory)))

(defn write-v
  ([{:keys [v] :as state} X val]
   (write-v state (assoc v X val)))
  ([{:keys [v] :as state} val]
   (assoc-in state [:v] (assoc-in-range v val))))

(def ^{:private true}
  default-state
  {;; CHIP-8 only has 4096 bytes (4KB) memory (0x0 ~ 0xFFF).
   ;;
   ;; The first 512 bytes, from 0x000 to 0x1FF, are where the
   ;; original interpreter was located, and should not used by programs.
   :memory (make-memory)

   ;; The stack is used to remember the current location
   ;; before a jump is performed. The system has 16 levels
   ;; of stack and in order to remember which level of the
   ;; stack is used.
   :stack  (vec (repeat 16 0))

   ;; The stack pointer (SP) is 8-bit, it is used to point
   ;; to the topmost level of the stack.
   :sp 0

   ;; Most CHIP-8 programs start at locastion 0x200 (512),
   ;; but some begin at 0x600 (1536).
   ;; The program counter (PC) should be 16-bit, and is used
   ;; to store the currently executing address.
   :pc 0x200

   ;; V-registers, CHIP-8 has 15 8-bit general purpose registers
   ;; named V0, V1 ~ VE. The 16th register is used for
   ;; the `VF flag`.
   :v (vec (repeat 16 0))

   ;; I-register is used to store memory address (Index register)
   :i 0

   ;; The delay timer is active whenever the delay timer
   ;; register (DT) is non-zero.
   ;; This timer does nothing more than subtract 1 from the
   ;; value of DT at a rate of 60Hz. When DT reaches 0, it deactivates.
   :delay-timer 0

   ;; The sound timer is active whenever the sound timer
   ;; register (ST) is non-zero. This timer also decrements
   ;; at a rate of 60Hz, however, as long as ST's value is
   ;; greater than zero, the Chip-8 buzzer will sound. When ST
   ;; reaches zero, the sound timer deactivates.
   :sound-timer 0

   ;; Graphics memory
   :screen (make-screen)

   :key 0  ;; store keycode if user enters a key
   })

(defn make-cpu []
  default-state)

(defn load-rom
  "Load rom to memory. The program will be loaded start at 0x200."
  [state rom]
  (-> state
      (write-memory (vec rom) 0x200)))

;; ------------------------------------------------------------------------------------------------------
;; ----------------------------------------- OPCODES START HERE -----------------------------------------
;; ------------------------------------------------------------------------------------------------------

;; NOTE: often the opcodes will end in: (add-to-pc 2), the reason we are advancing the program counter by 2
;; is that chip8 instructions are 2 bytes long, so we need to advance by 2 in order to continue to the next instruction.

(defn opcode-00E0
  "Clear the screen and move the program counter.
  NOTE: Incrementing the program counter by 2 continues to the next instruction."
  [{:keys [pc] :as state}]
  (-> state
      (write-screen (make-screen))
      (add-to-pc 2)))

(defn opcode-00EE
  "Return from a subroutine.
  The interpreter sets the program counter to the address at the top of the stack, then subtracts 1 from the stack pointer."
  [{:keys [sp stack] :as state}]
  (-> state
      (write-sp (dec sp))
      (write-pc (+ 2 (nth stack (dec sp))))))

(defn opcode-1NNN
  "Jump to address NNN.
  Basically set the program counter to point to address NNN."
  [state NNN]
  (-> state
      (write-pc NNN)))

(defn opcode-2NNN
  "Call subroutine at NNN.
  The interpreter increments the stack pointer, then puts the current PC on the
  top of the stack. The PC is then set to NNN."
  [{:keys [pc sp stack] :as state} NNN]
  (-> state
      (write-stack sp pc)
      (write-sp (inc sp))
      (write-pc NNN)))

;; NOTE: The next few instructions increment the program counter by 2 to continue normally to the next instruction. But, they increment it by an additional 2 (4) to skip the next instruction and then continue.

(defn opcode-3XNN
  "Skip next instruction if VX = NN.
  The interpreter compares register Vx to NN, and if they are equal, increments
  the program counter by 2."
  [{:keys [pc v] :as state} X NN]
  (let [Vx (nth v X)]
    (-> state
        (add-to-pc (if (= Vx NN) 4 2)))))

(defn opcode-4XNN
  "Skip next instruction if VX != NN.
  The interpreter compares register Vx to NN, and if they are not equal,
  increments the program counter by 2."
  [{:keys [pc v] :as state} X NN]
  (let [Vx (nth v X)]
    (-> state
        (add-to-pc (if-not (= Vx NN) 4 2)))))

(defn opcode-5XY0
  "Skip next instruction if Vx = Vy.
  The interpreter compares register Vx to register Vy, and if they are equal,
  increments the program counter by 2."
  [{:keys [pc v] :as state} X Y]
  (let [[Vx Vy] (VxVy v X Y)]
    (-> state
        (add-to-pc (if (= Vx Vy) 4 2)))))

(defn opcode-6XNN
  "Set Vx to NN.
  The interpreter puts the value NN into register Vx.
  Then it continues to the next instruction (increment PC)."
  [{:keys [pc] :as state} X NN]
  (-> state
      (write-v X NN)
      (add-to-pc 2)))

(defn opcode-7XNN
  "Set Vx = Vx + NN.
  Adds the value NN to the value of register Vx, then stores the result in Vx."
  [{:keys [pc v] :as state} X NN]
  (let [Vx (nth v X)]
    (-> state
        (write-v X (bit-and (+ Vx NN) 0xff)) ;; NOTE: We need the & 255, as V registers are supposed to be 8 bits long.
        (add-to-pc 2))))

(defn opcode-8XY0
  "Set Vx = Vy.
  Stores the value of register Vy in register Vx."
  [{:keys [pc v] :as state} X Y]
  (let [Vy (nth v Y)]
    (-> state
        (write-v X Vy)
        (add-to-pc 2))))

(defn opcode-8XY1
  "Set Vx = Vx OR Vy.
  Performs a bitwise OR on the values of Vx and Vy, then stores the
  result in Vx. A bitwise OR compares the corrseponding
  bits from two values, and if either bit is 1, then the same bit
  in the result is also 1. Otherwise, it is 0."
  [{:keys [pc v] :as state} X Y]
  (let [[Vx Vy] (VxVy v X Y)]
    (-> state
        (write-v  X (bit-or Vx Vy))
        (add-to-pc 2))))

(defn opcode-8XY2
  "Set Vx = Vx AND Vy.
  Performs a bitwise AND on the values of Vx and Vy, then stores
  the result in Vx. A bitwise AND compares the corrseponding
  bits from two values, and if both bits are 1, then the same
  bit in the result is also 1. Otherwise, it is 0."
  [{:keys [pc v] :as state} X Y]
  (let [[Vx Vy] (VxVy v X Y)]
    (-> state
        (write-v  X (bit-and Vx Vy))
        (add-to-pc 2))))

(defn opcode-8XY3
  "Set Vx = Vx XOR Vy.
  Performs a bitwise exclusive OR on the values of Vx and Vy, then
  stores the result in Vx. An exclusive OR compares the
  corrseponding bits from two values, and if the bits are not
  both the same, then the corresponding bit in the result is set
  to 1. Otherwise, it is 0."
  [{:keys [pc v] :as state} X Y]
  (let [[Vx Vy] (VxVy v X Y)]
    (-> state
        (write-v  X (bit-xor Vx Vy))
        (add-to-pc 2))))

(defn opcode-8XY4
  "Set Vx = Vx + Vy, set VF = carry.
  The values of Vx and Vy are added together. If the result is
  greater than 8 bits (i.e., > 255,) VF is set to 1,
  otherwise 0. Only the lowest 8 bits of the result are kept,
  and stored in Vx."
  [{:keys [pc v] :as state} X Y]
  (let [[Vx Vy] (VxVy v X Y)
        sum (+ Vx Vy)
        VF 15]
    (-> state
        (write-v  X (if (> sum 0xff) (- sum 256) sum))
        (write-v VF (if (> sum 0xff) 1 0))
        (add-to-pc 2))))

(defn opcode-8XY5
  "Set Vx = Vx - Vy, set VF = NOT borrow.
  If Vx > Vy, then VF is set to 1, otherwise 0. Then Vy is
  subtracted from Vx, and the results stored in Vx."
  [{:keys [pc v] :as state} X Y]
  (let [[Vx Vy] (VxVy v X Y)
        sub (- Vx Vy)]
    (-> state
        (write-v  X (if (< sub 0) (+ sub 256) sub))
        (write-v 15 (if (> sub 0) 1 0))
        (add-to-pc 2))))

(defn opcode-8XY6
  "Set Vx = Vx right-shift 1.
  If the least-significant bit of Vx is 1, then VF is set to 1,
  otherwise 0. Then Vx is divided by 2."
  [{:keys [pc v] :as state} X Y]
  (let [Vx (nth v X)]
    (-> state
        (write-v 15 (bit-and Vx 0x01))
        (write-v  X (bit-shift-right Vx 1))
        (add-to-pc 2))))

(defn opcode-8XY7
  "Set Vx = Vy - Vx, set VF = NOT borrow.
  If Vy > Vx, then VF is set to 1, otherwise 0. Then Vx is
  subtracted from Vy, and the results stored in Vx."
  [{:keys [pc v] :as state} X Y]
  (let [[Vx Vy] (VxVy v X Y)]
    (-> state
        (write-v 15 (if (> Vx Vy) 0 1))
        (write-v  X (- Vy Vx))
        (add-to-pc 2))))

(defn opcode-8XYE
  "Set Vx = Vx left-shift 1."
  [{:keys [pc v] :as state} X Y]
  (let [Vx (nth v X)]
    (-> state
        (write-v 15 (bit-and Vx 0x80))
        (write-v  X (bit-shift-left Vx 1))
        (add-to-pc 2))))

(defn opcode-9XY0
  "Skip next instruction if Vx != Vy."
  [{:keys [pc v] :as state} X Y]
  (let [[Vx Vy] (VxVy v X Y)]
    (-> state
        (add-to-pc (if-not (= Vx Vy) 4 2)))))

(defn opcode-ANNN
  "Set I = NNN."
  [{:keys [pc] :as state} NNN]
  (-> state
      (write-i NNN)
      (add-to-pc 2)))

(defn opcode-BNNN
  "Jump to location NNN + V0.
  Basically set the program counter to that location."
  [{:keys [v] :as state} NNN]
  (let [V0 (nth v 0)]
    (-> state
        (write-pc (+ V0 NNN)))))

(defn opcode-CXNN
  "Set Vx = random byte AND NN.
  The interpreter generates a random number from 0 to 255, which is then ANDed with the value kk. The results are stored in Vx."
  [{:keys [pc v] :as state} X NN]
  (-> state
      (write-v X (bit-and (rand-int 256) NN))
      (add-to-pc 2)))

(defn- protect-region
  "Prevent v ovrflow on bound."
  [v bound]
  (cond (> v (dec bound)) (protect-region (- v bound) bound)
        (< v 0)           (protect-region (+ v bound) bound)
        :else v))

(defn- set-pixel
  "Set the pixel on screen according x, y.
  Note that the pixel cooridinate is the same as CHIP-8 original
  implementation."
  [{{:keys [columns rows memory]} :screen :as state} x y & [val]]
  (let [nx (protect-region x columns)
        ny (protect-region y rows)
        v (get-in memory [nx ny])
        val-xor (bit-xor v 1)]
    (-> state
        (assoc-in [:screen :memory]
                  (assoc-in memory [nx ny] (or val val-xor)))
        ((fn [state]
           (if (= v 1)
             (write-v state 0xf 1) state))))))

;; ref: https://github.com/greatcodeclub/chip8/blob/master/vm.js
(defn opcode-DXYN
  "Display n-byte sprite starting at memory location I at (Vx, Vy),
  set VF = collision.
  The interpreter reads n bytes from memory, starting at the
  address stored in I. These bytes are then displayed as
  sprites on screen at coordinates (Vx, Vy). Sprites are XORed
  onto the existing screen. If this causes any pixels to be
  erased, VF is set to 1, otherwise it is set to 0. If the
  sprite is positioned so part of it is outside the coordinates
  of the display, it wraps around to the opposite side of the screen."
  [{:keys [pc v i memory] :as state} X Y N]
  (let [[Vx Vy] (VxVy v X Y)
        width 8
        height N]

    (-> state
        ;; clear VF before we start
        (write-v 0xf 0)
        ;; calculate uarr, a list contains coordinates which we need to update
        ;; the screen memory, we use loop recur to keep the immutable data
        ((fn [state]
           (let [uarr
                 (for [row (range height) :let [sprite (nth memory (+ i row))]
                       col (range width)  :let [sprite (bit-shift-left sprite col)]
                       :when (> (bit-and sprite 0x80) 0)]
                   [(+ Vx col) (+ Vy row)])]
             (if-not (empty? uarr)
               (loop [ar uarr
                      st state]
                 (if (empty? ar)
                   st
                   (recur (next ar)
                          (set-pixel st (nth (first ar) 0) (nth (first ar) 1)))))
               state))))
        (add-to-pc 2))))

(defn opcode-EX9E
  "Skip next instruction if key with the value of Vx is pressed."
  [{:keys [key v] :as state} X]
  (add-to-pc state (if (= key (nth v X)) 4 2)))

(defn opcode-EXA1
  "Skip next instruction if key with the value of Vx is not pressed."
  [{:keys [key v] :as state} X]
  (add-to-pc state (if-not (= key (nth v X)) 4 2)))

(defn opcode-FX07
  "Set Vx = delay timer value."
  [{:keys [pc v delay-timer] :as state} X]
  (-> state
      (write-v X delay-timer)
      (add-to-pc 2)))

(defn opcode-FX0A
  "Store the value of the currently pressed key in Vx.
  The value is in the range 0 - F"
  [{:keys [v] :as state} X]
  (let [Vx (nth v X)
        key (:key state)]
    (if-not (zero? key)
      (-> state
          (write-v X key)
          (add-to-pc 2))
      state)))

(defn opcode-FX15
  "Set delay timer = Vx."
  [{:keys [pc v] :as state} X]
  (let [Vx (nth v X)]
    (-> state
        (write-delay-timer Vx)
        (add-to-pc 2))))

(defn opcode-FX18
  "Set sound timer = Vx."
  [{:keys [pc v] :as state} X]
  (let [Vx (nth v X)]
    (-> state
        (write-sound-timer Vx)
        (add-to-pc 2))))

(defn opcode-FX29
  "Set I = location of sprite for digit Vx."
  [{:keys [pc v] :as state} X]
  (let [Vx (nth v X)]
    (-> state
        (write-i (* 5 Vx)) ;; NOTE: Each font value is made up of 5 bytes (see the def fonts above).
        (add-to-pc 2))))

(defn opcode-FX33
  "Store BCD representation of Vx in memory locations I, I+1, I+2."
  [{:keys [pc i v memory] :as state} X]
  (let [Vx (nth v X)]
    (-> state
        (write-memory (->bcd Vx) i)
        (add-to-pc 2))))

(defn opcode-FX55
  "Store registers V0 through Vx in memory starting at location I."
  [{:keys [pc i v memory] :as state} X]
  (-> state
      (write-memory (get-in-range v 0 (inc X)) i)
      (add-to-pc 2)))

(defn opcode-FX65
  "Read in registers V0 through Vx from memory starting at location I.
  The interpreter reads values from memory starting at location I into registers V0 through Vx."
  [{:keys [pc i v memory] :as state} X]
  (-> state
      (write-v (get-in-range memory i (+ i X 1)))
      (add-to-pc 2)))

(defn opcode-FX1E
  "Set I = I + Vx."
  [{:keys [pc i v] :as state} X]
  (let [Vx (nth v X)]
    (-> state
        (write-i (+ i Vx))
        (add-to-pc 2))))

(defn emulation-cycle
  "Gets the next instruction from memory and dispatches the proper opcode function to handle it."
  [{:keys [memory pc] :as state}]
  (let [opcode (str (int-to-hex-str (nth memory pc))
                    (int-to-hex-str (nth memory (inc pc))))
        [w x y z] opcode ;; Deconstruct the opcode into 4 bit pieces.
        NNN (hex-to-int (str x y z))
        NN  (hex-to-int (str y z))
        X   (hex-to-int (str x))
        Y   (hex-to-int (str y))
        N   (hex-to-int (str z))]
    (match [w  x  y  z]
           [\0 \0 \e \0] (opcode-00E0 state)
           [\0 \0 \e \e] (opcode-00EE state)
           [\1  _  _  _] (opcode-1NNN state NNN)
           [\2  _  _  _] (opcode-2NNN state NNN)
           [\3  _  _  _] (opcode-3XNN state X NN)
           [\4  _  _  _] (opcode-4XNN state X NN)
           [\5  _  _  _] (opcode-5XY0 state X Y)
           [\6  _  _  _] (opcode-6XNN state X NN)
           [\7  _  _  _] (opcode-7XNN state X NN)
           [\8  _  _ \0] (opcode-8XY0 state X Y)
           [\8  _  _ \1] (opcode-8XY1 state X Y)
           [\8  _  _ \2] (opcode-8XY2 state X Y)
           [\8  _  _ \3] (opcode-8XY3 state X Y)
           [\8  _  _ \4] (opcode-8XY4 state X Y)
           [\8  _  _ \5] (opcode-8XY5 state X Y)
           [\8  _  _ \6] (opcode-8XY6 state X Y)
           [\8  _  _ \7] (opcode-8XY7 state X Y)
           [\8  _  _ \e] (opcode-8XYE state X Y)
           [\9  _  _ \0] (opcode-9XY0 state X Y)
           [\a  _  _  _] (opcode-ANNN state NNN)
           [\b  _  _  _] (opcode-BNNN state NNN)
           [\c  _  _  _] (opcode-CXNN state X NN)
           [\d  _  _  _] (opcode-DXYN state X Y N)
           [\e  _ \9 \e] (opcode-EX9E state X)
           [\e  _ \a \1] (opcode-EXA1 state X)
           [\f  _ \0 \7] (opcode-FX07 state X)
           [\f  _ \0 \a] (opcode-FX0A state X)
           [\f  _ \1 \5] (opcode-FX15 state X)
           [\f  _ \1 \8] (opcode-FX18 state X)
           [\f  _ \2 \9] (opcode-FX29 state X)
           [\f  _ \3 \3] (opcode-FX33 state X)
           [\f  _ \5 \5] (opcode-FX55 state X)
           [\f  _ \6 \5] (opcode-FX65 state X)
           [\f  _ \1 \e] (opcode-FX1E state X)
           ;; If we enter here, there's something wrong :(
           :else (throw (IllegalArgumentException. (str "Illegal opcode: " w x y z))))))

(defn update-delay-timer
  [{:keys [delay-timer] :as state}]
  (if (> delay-timer 0)
    (write-delay-timer state (- delay-timer 1))
    state))

(defn update-sound-timer
  [{:keys [sound-timer] :as state}]
  (if (> sound-timer 0)
    (write-sound-timer state (- sound-timer 1))
    state))

(defn update-timers [state]
  (-> state
      (update-delay-timer)
      (update-sound-timer)))

(defn step
  "Executes one cycle of the emulation loop.
  Notice that this functions limits the emulator's execution speed
  regardless of the FPS that Quil is set to."
  ([state speed]
   (-> (loop [s state
              acc 0]
         (if (> acc speed)
           s
           (recur (emulation-cycle s) (inc acc))))
       (update-timers)))
  ([state]
   (step state 1)))

;; ------------------------------------------------------------------------------------------------------
;; -------------------------------------------- GUI PORTION ---------------------------------------------
;; ------------------------------------------------------------------------------------------------------

(def chip8-state (atom (make-cpu)))
(def current-rom (atom ""))

(def speed 11)
(def fps 60)
(def screen-width 640)
(def screen-height 320)
;; Note the original chip8 monochrome display had 64x32 pixels,
;; we are using 640x320 pixels, so that's a scale of 10.
(def scale 10)

(defn setup []
  (q/frame-rate fps)
  (q/stroke 0)
  (q/stroke-weight 0)
  (q/background 20 20 20))

(defn draw-state!
  "Render the canvas according to graphics memory."
  [{{:keys [columns rows memory]} :screen :as state}]
  (q/background 20) ;; Clear the canvas before each draw.
  (dotimes [x columns]
    (dotimes [y rows]
      (when-not (zero? (get-in memory [x y]))
        (q/fill 255 255 255)
        (q/rect (* x scale) (* y scale) scale scale)))))

(defn emulation-loop! []
  (swap! chip8-state #(step % speed))
  (draw-state! @chip8-state))

(defn get-key []
  (let [raw-key (q/raw-key)
        the-key-code (q/key-code)
        the-key-pressed (if (= processing.core.PConstants/CODED (int raw-key)) the-key-code raw-key)]
    the-key-pressed))

;; NOTE: the original computers that used chip8 had a hexadecimal pad as their imput (the keys from 0 to F).
;; This is not convenient on modern keyboards, so we are going to use the left hand side of the QWERTY.
(defn read-keyboard []
  (let [user-input (get-key)]
    (cond
      (= \1 user-input) (swap! chip8-state #(write-register % :key 0x01))
      (= \2 user-input) (swap! chip8-state #(write-register % :key 0x02))
      (= \3 user-input) (swap! chip8-state #(write-register % :key 0x03))
      (= \4 user-input) (swap! chip8-state #(write-register % :key 0x0C))
      (= \q user-input) (swap! chip8-state #(write-register % :key 0x04))
      (= \w user-input) (swap! chip8-state #(write-register % :key 0x05))
      (= \e user-input) (swap! chip8-state #(write-register % :key 0x06))
      (= \r user-input) (swap! chip8-state #(write-register % :key 0x0D))
      (= \a user-input) (swap! chip8-state #(write-register % :key 0x07))
      (= \s user-input) (swap! chip8-state #(write-register % :key 0x08))
      (= \d user-input) (swap! chip8-state #(write-register % :key 0x09))
      (= \f user-input) (swap! chip8-state #(write-register % :key 0x0E))
      (= \z user-input) (swap! chip8-state #(write-register % :key 0x0A))
      (= \x user-input) (swap! chip8-state #(write-register % :key 0x00))
      (= \c user-input) (swap! chip8-state #(write-register % :key 0x0B))
      (= \v user-input) (swap! chip8-state #(write-register % :key 0x0F))
      ;; Restart the emulator when the user presses enter.
      (= \newline user-input) (reset! chip8-state (-> (make-cpu) (load-rom (file-to-bytes @current-rom)))))))

(defn -main [argv]
  ;; The ROM to play must be specified as a command line argument.
  (reset! current-rom argv)
  (swap! chip8-state #(-> % (load-rom (file-to-bytes @current-rom))))
  (q/defsketch chip8
    :title "Chip8"
    :settings #(q/smooth 2)
    :setup setup
    :renderer :opengl
    :key-pressed read-keyboard
    :key-released (fn [] (swap! chip8-state #(write-register % :key 0)))
    :draw emulation-loop!
    :features [:exit-on-close]
    :size [screen-width screen-height]))
