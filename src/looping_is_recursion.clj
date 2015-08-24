(ns looping-is-recursion)

(defn power [base exp]
  (letfn [(helper [curr base exp]
    (if (zero? exp)
      curr
      (recur (* curr base) base (dec exp))))]
    (helper 1 base exp)))


(defn last-element [a-seq]
  (letfn [(helper [curr rst]
    (if (empty? rst)
      curr
      (recur (first rst) (rest rst))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (letfn [(helper [rst1 rst2]
    (cond
       (and  (empty? rst1) (empty? rst2)) true
       (or   (empty? rst1) (empty? rst2)) false
       (not= (first rst1)  (first rst2) ) false
       :else (recur (rest rst1) (rest rst2))))]
    (helper seq1 seq2)))


(defn find-first-index [pred a-seq]
  (loop [index 0
         rst a-seq]
    (cond
     (empty? rst) nil
     (pred (first rst)) index
     :else (recur (inc index) (rest rst)))))

(defn avg [a-seq]
  (loop [index 0
         sum 0
         rst a-seq]
    (if (empty? rst)
      (/ sum index)
      (recur (inc index) (+ (first rst) sum) (rest rst)))))

(defn parity [a-seq]
  (letfn [(toggle [a-set elem]
     (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))]
   (loop [rst a-seq
          parities #{}]
     (if (empty? rst)
         parities
         (recur (rest rst) (toggle parities (first rst)))))))

(defn fast-fibo [n]
  (loop [togo n
         n1 0
         n2 1]
    (if (zero? togo)
      n1
      (recur (dec togo) n2 (+ n1 n2)))))

(defn cut-at-repetition [a-seq]
  (letfn [(toggle [a-set elem]
     (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))]
    (loop [rst a-seq
           del #{}
           res []]
      (cond
       (empty? rst) res
       (contains? del (first rst)) res
       :else (recur (rest rst) (toggle del (first rst)) (conj res (first rst)))))))
