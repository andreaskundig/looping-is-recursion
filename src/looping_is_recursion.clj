(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [latest-element a-seq]
                 (if (empty? a-seq)
                   latest-element
                   (recur (first a-seq) (next a-seq))))]
    (helper nil a-seq)))

(defn last-element2 [coll]
  (cond (empty? coll) nil
        (empty? (rest coll)) (first coll)
        :else (recur (rest coll))))

(defn seq= [seq1 seq2]
  (if (or (empty? seq1) (empty? seq2)) 
    (and (empty? seq1) (empty? seq2))
    (and  (= (first seq1) (first seq2))
          (recur (rest seq1) (rest seq2)))))

(defn find-first-index [pred a-seq]
  (loop [index 0
         rest-seq a-seq]
    (cond (empty? rest-seq) nil
          (pred (first rest-seq)) index
          :else (recur (inc index) (rest rest-seq)))))

(defn avg [a-seq]
  (loop [total 0
         count 0
         a-seq a-seq]
    (if (empty? a-seq)
      (/ total count)
      (recur (+ total (first a-seq))(inc count) (rest a-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (apply disj [a-set elem])
    (apply conj [a-set elem])))

(defn parity [a-seq]
  (loop [result #{}
         a-seq a-seq]
    (if (empty? a-seq) 
      result
      (recur (toggle result (first a-seq)) (rest a-seq)))))

(defn fast-fibo [n]
  (loop [current-n 2
         fibo-1 1
         fibo-2 0]
    (cond
     (= n 0 ) fibo-2
     (= n 1) fibo-1
     (= n current-n) (+ fibo-1 fibo-2)
     :else (recur (inc current-n) (+ fibo-1 fibo-2) fibo-1))))

(defn cut-at-repetition [a-seq]
  (loop [seen #{} 
         result []
         remaining-seq a-seq]
    (let [next-value (first remaining-seq)]
      (if (or (empty? remaining-seq) (contains? seen next-value))
        result
        (recur (conj seen next-value)
               (conj result next-value)
               (rest remaining-seq))))))

