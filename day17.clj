(ns aoc2024.day17
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [aoc2024.utils :as au]
            [aoc2024.clerk :as clrk]
            ))

;;; Oh my god this one killed me for some reason


(defn execute
  [a b c program]
  (let [a (atom a) b (atom b) c (atom c)
        p (atom 0)
        ->a #(reset! a %)
        ->b #(reset! b %)
        ->c #(reset! c %)
        out (atom [])
        ->out #(swap! out conj %)
        literal #(nth program (inc @p))
        combo #(case (nth program (inc @p))
                 0 0 1 1 2 2 3 3
                 4 @a 5 @b 6 @c)
        adv #(long (/ @a
                      (int (Math/pow 2 (combo)))))
        ]
    (loop [pp 0]
      (reset! p pp)
      (prn :p pp @out)
      (cond
        (>= pp (count program)) @out
        (and (= 3 (nth program pp))
               (not (zero? @a)))
        (recur (literal))
        :else
        (do
          (case (nth program pp)
            ;; The adv instruction (opcode 0) performs division. The numerator is the value in the A register. The denominator is found by raising 2 to the power of the instruction's combo operand. (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.) The result of the division operation is truncated to an integer and then written to the A register.
            0 (->a
               (adv))
            ;; The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the instruction's literal operand, then stores the result in register B.
            1 (->b (bit-xor @b (literal)))
            ;; The bst instruction (opcode 2) calculates the value of its combo operand modulo 8 (thereby keeping only its lowest 3 bits), then writes that value to the B register.
            2 (->b (mod (combo) 8))
            ;; The jnz instruction (opcode 3) does nothing if the A register is 0. However, if the A register is not zero, it jumps by setting the instruction pointer to the value of its literal operand; if this instruction jumps, the instruction pointer is not increased by 2 after this instruction.
            3 nil
            ;;The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C, then stores the result in register B. (For legacy reasons, this instruction reads an operand but ignores it.)
            4 (->b (bit-xor @b @c))

            ;; The out instruction (opcode 5) calculates the value of its combo operand modulo 8, then outputs that value. (If a program outputs multiple values, they are separated by commas.)
            5 (->out (mod (combo) 8))

            ;; The bdv instruction (opcode 6) works exactly like the adv instruction except that the result is stored in the B register. (The numerator is still read from the A register.)
            6  (->b (adv))

            ;; The cdv instruction (opcode 7) works exactly like the adv instruction except that the result is stored in the C register. (The numerator is still read from the A register.))
            7  (->c (adv)))
          (recur (+ pp 2))
          )))))

(defn test
  []
  (execute 729 0 0
           [0,1,5,4,3,0]))

;;; The same but output either matches the program or errors out
(defn execute2
  [a b c program]
  (let [a (atom a) b (atom b) c (atom c)
        p (atom 0)
        ->a #(reset! a %)
        ->b #(reset! b %)
        ->c #(reset! c %)
        out (atom [])
        ->out #(do (swap! out conj %)
                   #_ (prn :out %)
                   (when-not (= @out (subvec program 0 (count @out)))
                     (prn :nope @out)
                     (throw (ex-info "nope" {:out @out}))))
        literal #(nth program (inc @p))
        combo #(case (nth program (inc @p))
                 0 0 1 1 2 2 3 3
                 4 @a 5 @b 6 @c)
        adv #(long (/ @a
                      (int (Math/pow 2 (combo)))))
        ]
    (loop [pp 0]
      (reset! p pp)
      #_ (prn [(au/str-binary @a) (au/str-binary @b) (au/str-binary @c)] pp)
      (cond
        (>= pp (count program)) @out
        (and (= 3 (nth program pp))
               (not (zero? @a)))
        (recur (literal))
        :else
        (do
          (case (nth program pp)
            ;; The adv instruction (opcode 0) performs division. The numerator is the value in the A register. The denominator is found by raising 2 to the power of the instruction's combo operand. (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.) The result of the division operation is truncated to an integer and then written to the A register.
            0 (->a
               (adv))
            ;; The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the instruction's literal operand, then stores the result in register B.
            1 (->b (bit-xor @b (literal)))
            ;; The bst instruction (opcode 2) calculates the value of its combo operand modulo 8 (thereby keeping only its lowest 3 bits), then writes that value to the B register.
            2 (->b (mod (combo) 8))
            ;; The jnz instruction (opcode 3) does nothing if the A register is 0. However, if the A register is not zero, it jumps by setting the instruction pointer to the value of its literal operand; if this instruction jumps, the instruction pointer is not increased by 2 after this instruction.
            3 nil
            ;;The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C, then stores the result in register B. (For legacy reasons, this instruction reads an operand but ignores it.)
            4 (->b (bit-xor @b @c))

            ;; The out instruction (opcode 5) calculates the value of its combo operand modulo 8, then outputs that value. (If a program outputs multiple values, they are separated by commas.)
            5 (->out (mod (combo) 8))

            ;; The bdv instruction (opcode 6) works exactly like the adv instruction except that the result is stored in the B register. (The numerator is still read from the A register.)
            6  (->b (adv))

            ;; The cdv instruction (opcode 7) works exactly like the adv instruction except that the result is stored in the C register. (The numerator is still read from the A register.))
            7  (->c (adv)))
          (recur (+ pp 2))
          )))))

(defn p1
  []
  (s/join "," (execute 46187030 0 0 [2,4,1,5,7,5,0,3,4,0,1,6,5,5,3,0])))

(defn execute-a
  [a]
  (execute2 a 0 0 [2,4,1,5,7,5,0,3, 4,0,1,6,5,5,3,0]))

;;; Nope brute forcing
(defn p2
  []
  (doseq [a (range 200000000)]
    (u/ignore-errors
      (let [res  (execute2 a 0 0 [2,4,1,5,7,5,0,3, 4,0,1,6,5,5,3,0])]
        (prn :foo (au/str-binary a) res)
        (when (= (count res)
                 16)
          (prn :a a))))))

;; 2,4  a mod 8 → b 
;; 1,5  b xor 5 → b
;; 7,5  a / 2^b → c    Thats a right shift
;; 0,3  a / 8 → a      Also shift right (3)  
;; 4,0  b xor c → b
;; 1,6  b xor 6 → b
;; 5,5  OUT b mod 8     
;; 3,0  jmp to 0 if a != 0



:foo 1 [2]
:foo 2203 [2 4 1 5]
:foo 2458 [2 4 1 5]
:foo 3841 [2 4 1 5]
:foo 3849 [2 4 1 5]
:foo 12093850 [2 4 1 5 7 5 0 3]
:foo 12095233 [2 4 1 5 7 5 0 3]
:foo 12095241 [2 4 1 5 7 5 0 3]
:foo 12120841 [2 4 1 5 7 5 0 3]
:foo 12159386 [2 4 1 5 7 5 0 3]
:foo 12160769 [2 4 1 5 7 5 0 3]
:foo 12160777 [2 4 1 5 7 5 0 3]
:foo                        "1" [2]
:foo             "100010011011" [2 4 1 5]
:foo             "100110011010" [2 4 1 5]
:foo             "111100000001" [2 4 1 5]
:foo             "111100001001" [2 4 1 5]
:foo "101110001000100110011010" [2 4 1 5 7 5 0 3]
:foo "101110001000111100000001" [2 4 1 5 7 5 0 3]
:foo "101110001000111100001001" [2 4 1 5 7 5 0 3]
:foo "101110001111001100001001" [2 4 1 5 7 5 0 3]
:foo "101110011000100110011010" [2 4 1 5 7 5 0 3]
:foo "101110011000111100000001" [2 4 1 5 7 5 0 3]
:foo "101110011000111100001001" [2 4 1 5 7 5 0 3]


;;; Somewhat less brutal forcing
;;; a is int, x is 3-bit tag to prepend
(defn prepend
  [x a]
  (au/read-binary
   (str
    (au/str-binary x)                   ;pad?
    (au/str-binary a))))

(defn prepend2
  [a x]
  (au/read-binary
   (str
    (au/str-binary x)                   ;pad?
    a)))

(defn p2x
  [start]
  (doseq [x (range 100000)]
    #_ (prn :x x)
    (let [start (prepend2 start x)]
      (u/ignore-errors
        (let [res (execute2 start 0 0 [2,4,1,5,7,5,0,3, 4,0,1,6,5,5,3,0])]
          (prn :foo (au/str-binary start) res)
          (when (= (count res)
                   16)
            (prn :a start)))))))

21671479706

(defn execute3
  [a b c program]
  (let [a (atom a) b (atom b) c (atom c)
        p (atom 0)
        ->a #(reset! a %)
        ->b #(reset! b %)
        ->c #(reset! c %)
        out (atom [])
        ->out #(do (swap! out conj %)
                   #_ (prn :out %)
                   #_ (when-not (= @out (subvec program 0 (count @out)))
                        (prn :nope @out)
                        (throw (ex-info "nope" {:out @out}))))
        literal #(nth program (inc @p))
        combo #(case (nth program (inc @p))
                 0 0 1 1 2 2 3 3
                 4 @a 5 @b 6 @c)
        adv #(long (/ @a
                      (int (Math/pow 2 (combo)))))
        ]
    (loop [pp 0]
      (reset! p pp)
      #_ (prn [(au/str-binary @a) (au/str-binary @b) (au/str-binary @c)] pp)
      (cond
        (>= pp (count program)) @out
        (and (= 3 (nth program pp))
               (not (zero? @a)))
        (recur (literal))
        :else
        (do
          (case (nth program pp)
            ;; The adv instruction (opcode 0) performs division. The numerator is the value in the A register. The denominator is found by raising 2 to the power of the instruction's combo operand. (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.) The result of the division operation is truncated to an integer and then written to the A register.
            0 (->a
               (adv))
            ;; The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the instruction's literal operand, then stores the result in register B.
            1 (->b (bit-xor @b (literal)))
            ;; The bst instruction (opcode 2) calculates the value of its combo operand modulo 8 (thereby keeping only its lowest 3 bits), then writes that value to the B register.
            2 (->b (mod (combo) 8))
            ;; The jnz instruction (opcode 3) does nothing if the A register is 0. However, if the A register is not zero, it jumps by setting the instruction pointer to the value of its literal operand; if this instruction jumps, the instruction pointer is not increased by 2 after this instruction.
            3 nil
            ;;The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C, then stores the result in register B. (For legacy reasons, this instruction reads an operand but ignores it.)
            4 (->b (bit-xor @b @c))

            ;; The out instruction (opcode 5) calculates the value of its combo operand modulo 8, then outputs that value. (If a program outputs multiple values, they are separated by commas.)
            5 (->out (mod (combo) 8))

            ;; The bdv instruction (opcode 6) works exactly like the adv instruction except that the result is stored in the B register. (The numerator is still read from the A register.)
            6  (->b (adv))

            ;; The cdv instruction (opcode 7) works exactly like the adv instruction except that the result is stored in the C register. (The numerator is still read from the A register.))
            7  (->c (adv)))
          (recur (+ pp 2))
          )))))


(defn x3
  [a]
  (execute3 a 0 0 [2,4,1,5,7,5,0,3, 4,0,1,6,5,5,3,0]))


(defn p3
  []
  (filter #(= 2 (first (x3 %)))
          (range 2048)))

(defn subvec=
  [s v]
  (and (>= (count v) (count s))
       (= s (subvec v 0 (count s)))))

(defn p32
  []
  (filter #(subvec= [2 4] (x3 %))
          (for [i (range 8)
                p p3x]
            (prepend i p))))

(defn foo
  [] 
  (filter #(subvec= [2 4] (x3 %)) (range 4096)))

(defn foo2
  [] 
  (map #(x3 (prepend 777 % ))
       (range 8)))

(defn pfpt                          ;I am getting punchy
  [candidates subvec]
  (filter #(subvec= subvec (x3 %))
          (for [i (range 8)
                p candidates]
            (prepend i p))))

(defn snik
  []
  (loop [candidates p3xx
         i 2]
    (let [svec (subvec [2,4,1,5,7,5,0,3, 4,0,1,6,5,5,3,0] 0 i)]
      (prn :x i (count candidates) svec (take 5 candidates))
      (recur (pfpt candidates svec) (inc i)))))

(defn prepend3
  [x a]
  (au/read-binary
   (str
    (au/str-binary x)                   ;pad?
    a)))

(defn pfpt2                          ;I am getting punchy
  [candidates subvec size]
  (filter #(subvec= subvec (x3 %))
          (for [i (range 8)
                p candidates]
            (prepend3 i (au/str-binary-pad p size)))))


(def program  [2,4,1,5,7,5,0,3, 4,0,1,6,5,5,3,0])

(defn snik2
  []
  (loop [candidates (filter #(subvec= [2] (x3 %))
                            (range 1024))
         s 10
         i 1]
    (let [svec (if (= i 17) program (subvec program 0 i))]
      (prn :x i s (count candidates) svec (take 5 candidates))
      (recur (pfpt2 candidates svec s) (+ s 3) (inc i)))))

