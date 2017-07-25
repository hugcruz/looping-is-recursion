(ns looping-is-recursion)


(defn power-n [base exp acc]
  (if (= exp 1)
    (* acc base)
    (recur base (dec exp) (* acc base))))

(defn power [base exp]
    (if (= exp 0)
      1
      (power-n base exp 1)))

(defn last-element [a-seq]
  (if (<= (count a-seq) 1)
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (not= (count seq1) (count seq2)) false
    (not= (first seq1) (first seq2)) false
    (empty? seq1)                    true
    :else                            (recur (rest seq1) (rest seq2))
  ))

(defn find-first-index [pred a-seq]
  (loop [
          idx    0
          my-seq a-seq
        ]
    (cond
      (empty? my-seq)        nil
      (pred (first my-seq)) idx
      :else                 (recur (inc idx) (rest my-seq))
    )))

(defn avg [a-seq]
  (loop [
          acc    0
          my-seq a-seq
         ]
    (if (empty? my-seq)
      (/ acc (count a-seq))
      (recur (+ acc (first my-seq)) (rest my-seq))
)))

(defn parity [a-seq]
  ":(")

(defn fast-fibo [n]
  ":(")

(defn cut-at-repetition [a-seq]
  [":("])

