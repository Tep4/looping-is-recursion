(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   1
                   (if (< 1 n)
                     (recur (* base acc) (dec n))
                     acc)))]
    (helper base exp)))

(defn last-element [a-seq]
  (let [helper (fn [a-seq]
                 (if (empty? a-seq)
                   nil
                   (if (empty? (rest a-seq))
                     (first a-seq)
                     (recur (rest a-seq)))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [a-seq b-seq]
                 (if (and (empty? a-seq) (empty? b-seq))
                   true
                   (if (or (empty? a-seq) (empty? b-seq))
                     false
                     (if (=
                           (first a-seq)
                           (first b-seq))
                       (recur (rest a-seq) (rest b-seq))
                       false))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         seq1 a-seq]
    (if (empty? seq1)
      nil
      (if (pred (first seq1))
        acc
        (recur (inc acc) (rest seq1))))))

(defn avg [a-seq]
  (loop [acc 0
         counter 0
         seq1 a-seq]
    (if (empty? seq1)
      (/ acc counter)
      (recur (+ acc (first seq1)) (inc counter) (rest seq1)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem] (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))]
    (loop [res #{}
         seq1 a-seq]
    (if (empty? seq1)
      res
      (recur (toggle res (first seq1)) (rest seq1))))))

(defn fast-fibo [n]
  (loop [fib 1
         fib-1 0
         target n
         counter 2]
    (if (zero? target)
      0
      (if (= target 1)
        1
        (if (= counter target)
          (+ fib fib-1)
          (recur (+ fib fib-1) fib target (inc counter)))))))

(defn cut-at-repetition [a-seq]
  (loop [res []
         seq1 a-seq]
    (if (empty? seq1)
      res
      (if (contains? (set res) (first seq1))
        res
        (recur (conj res (first seq1)) (rest seq1))))))

