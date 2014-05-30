(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce
     (fn [a s] (str a " " s))
     a-seq)))
     

(defn my-interpose [x a-seq]
  (cond
   (empty? a-seq)        '()
   (empty? (rest a-seq)) (apply list a-seq)
   :else (reduce
                (fn [acc elem] (conj (conj acc x) elem))
                [(first a-seq)]
                (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [acc s] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc s] (cons s acc)) '() a-seq))

(defn min-max-element [a-seq]
    (reduce (fn [[mn mx] e] [(min mn e) (max mx e)])
            [(first a-seq) (first a-seq)]
            (rest a-seq)))
          

(defn insert [sorted-seq n]
  (cond
   (empty? sorted-seq)            (list n)
   (< n (first sorted-seq))       (cons n sorted-seq)
   :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))
          
(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce (fn [st e] (toggle st e))
            #{}
            a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([]         1)
  ([x]        x)
  ([x & more] (reduce * x more)))

(defn pred-and
  ([]             (fn [&more] true))
  ([pred]         pred)
  ([pred & preds] (reduce (fn [acc p] (fn [x] (and (acc x) (p x))))
                          pred
                          preds)))

(defn my-map [f & a-seq]
  (let [my-map-1   map
        my-map-2   map
        transpose  (fn [a-seq] (reduce (partial my-map-2 conj) (my-map-1 vector (first a-seq))   (rest a-seq)))]
    (my-map-1 #(apply f %) (transpose a-seq))))

; (defn my-map-1 [f a-seq]
;   (if (empty? a-seq)
;     '()
;     (cons (f (first a-seq)) (my-map-1 f (rest a-seq)))))
; 
; (defn my-map-2 [f a-seq b-seq]
;   (if (or (empty? a-seq) (empty? b-seq))
;     '()
;     (cons (f (first a-seq) (first b-seq)) (my-map-2 f (rest a-seq) (rest b-seq)))))
