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

(defn toggle [a-set elem]
  (if (contains? a-set elem)
   (disj a-set elem)
   (conj a-set elem)
))

(defn parity [a-seq]
  (loop [
          results #{}
          my-seq  a-seq
        ]
    (if (empty? my-seq)
      results
      (recur (toggle results (first my-seq)) (rest my-seq))
)))


(defn fast-fibo [n]
  (loop [
          cnt 2
          n1  1
          n2  0
        ]
    (cond
      (= n   0) 0
      (= n   1) 1
      (= cnt n) (+ n1 n2)
      :else     (recur (inc cnt) (+ n1 n2) n1)
)))

(defn cut-at-repetition [a-seq]
  (loop [
          results []
          my-seq  a-seq
        ]
    (cond
      (empty? my-seq)
        results
      (some #(= (first my-seq) %) results)
        (recur results (rest my-seq))
      :else
        (recur (conj results (first my-seq)) (rest my-seq))
)))

