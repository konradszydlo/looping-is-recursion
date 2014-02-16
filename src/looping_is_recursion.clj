(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) (dec n))))]
     (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [elem tmp-seq]
                 (if (empty? tmp-seq)
                   elem
                   (recur (first tmp-seq) (rest tmp-seq))))]
    (helper (first a-seq) (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (or (empty? seq1) (empty? seq2)) false
   (not (= (first seq1) (first seq2))) false
   :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [n 0 tmp-seq a-seq]
    (cond
     (empty? tmp-seq) nil
     (pred (first tmp-seq)) n
     :else (recur (inc n) (rest tmp-seq)))))

(defn avg [a-seq]
  (loop [cur-sum 0 tmp-seq a-seq]
    (if (empty? tmp-seq)
      (/ cur-sum (count a-seq) )
      (recur (+ cur-sum (first tmp-seq)) (rest tmp-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [cur-set #{} tmp-seq a-seq]
    (if (empty? tmp-seq)
      cur-set
      (recur (toggle cur-set (first tmp-seq)) (rest tmp-seq)))))

(defn fast-fibo [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else
   (loop [n (dec n) cur-fb 1 fb1 0]
     (if (= n 0)
       cur-fb
       (recur (dec n) (+ cur-fb fb1) cur-fb)))))

; this version is faster then the following.
(defn cut-at-repetition [a-seq]
  (loop [tmp-vec [] tmp-seq a-seq]
    (if (or (empty? tmp-seq) (contains? (set tmp-vec) (first tmp-seq)))
      tmp-vec
      (recur (conj tmp-vec (first tmp-seq)) (rest tmp-seq)))))

(defn cut-at-repetition1 [a-seq]
  (loop [elems #{}
         new []
         s a-seq]
    (if (or (empty? s) (contains? elems (first s)))
      new
      (recur (conj elems (first s))
             (conj new (first s))
             (rest s)))))

(time (cut-at-repetition [0 1 2 3 4 5 6 7 8 9 10]))

(time ( cut-at-repetition1 [0 1 2 3 4 5 6 7 8 9 10]))
