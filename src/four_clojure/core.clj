(ns four-clojure.core)

;; Problem #70
;; Write a function that splits a sentence up into a sorted list of words.
;; Capitalization should not affect sort order and punctuation should be ignored.
;; e.g. "Have a nice day." --> ["a" "day" "have" "nice"]
  (defn my-sort [s]
  (sort 
   #(compare (clojure.string/lower-case %1) 
             (clojure.string/lower-case %2)) 
   (clojure.string/split 
    (apply str (remove #((set ",;.") %) s)) #" ")))

;; Day 4
;; Problem #27
(= (reverse (seq %)) (seq %))


;; #32
(defn dup [l] 
  (loop [in l result l]
    (if (empty? in)
      (sort  result)
      (recur (rest in) (conj result (first in))))))


;; #30
(defn p30 [l]
  (let [res-atom (atom '())] 
    (doseq [x l]
      (when (not= (first @res-atom) x)
        (swap! res-atom (fn [c] (conj c x))))) 
    (reverse @res-atom)))

;; #31
#(partition-by identity %)

;; #41
(fn [l n] (vec (filter #(not= (mod (+ (.indexOf l %) 1) n) 0) l)))

;; #33
(defn p33 [l n]
  (apply concat (map #(take n (iterate identity %)) l)))

;; #26
(defn p26 [n]
  (take n
        (map first (iterate
                    (fn [[a b]] 
                      [b (+ a b)]) [1 1]))))

;; #29
(defn p29 [s]
  (apply str (filter #(Character/isUpperCase %) s)))

;; #42
(defn p42 [n]
  (loop [i 1
         result 1]
    (if (= i n)
      result
      (recur (inc i) (* result (inc i))))))

;; #83
(defn p83 [& b]
  (boolean (and (some true? b) (some false? b))))

;; #66
(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (mod a b))))

;; #107 
(defn p107 [exponent]
  (fn [base]
    (reduce * 1 (repeat exponent base))))

;; #90
(defn descartes [a b]
  (set (mapcat (fn [c]
                  (map (fn [d] [c d]) b)) a)))


;; Example of manipulating an atom in a multithreaded context
(def counter (atom 0))

(defn inc-print [v]
  (println v)
  (inc v))

(let [n 2]
  (future (dotimes [_ n] (swap! counter inc-print)))
  (future (dotimes [_ n] (swap! counter inc-print)))
  (future (dotimes [_ n] (swap! counter inc-print))))

;; dosync example
(def alice-height (ref 3))
(def right-hand-bites (ref 10))

(defn eat-from-right-hand []
  (dosync (when (pos? @right-hand-bites)
            (alter right-hand-bites dec)
            (alter alice-height #(+ % 24)))))

(let [n 2]
  (future (dotimes [_ n] (eat-from-right-hand)))
  (future (dotimes [_ n] (eat-from-right-hand)))
  (future (dotimes [_ n] (eat-from-right-hand))))

@alice-height
@right-hand-bites

;; p88
(defn p88 [s1 s2]
  (clojure.set/union (clojure.set/difference s1 s2) (clojure.set/difference s2 s1)))

;; #100
(defn p100 [& b]
  (loop [i b
         result []]
    (if (empty? i)
      (apply min (apply clojure.set/intersection result)) 
      (recur (rest i)
             (conj result (set (map (partial * (first i)) (range 1 500))))))))

;; #97
(defn p97 [n]
  (let [parse (fn p [l res]
                (if (empty? l)
                  res
                  (p (rest l) (do
                                    (if (> (count l) 1)
                                      (conj res (+ (first l) (second l)))
                                      (conj res 1))))))]
    (loop [i n
           arr [[1]]]
      (if (zero? i)
        (last (take n arr))
        (recur (dec i) (conj arr (parse (last arr) [1])))))))

;; #95
(defn btree [s]
  (if (nil? s)
    true
    (do
      (if (false? s)
        false
        (do
          (if (not= (count s) 3)
            false
            (and (btree (second s)) (btree (nth s 2)))))))))

;; #96
(defn symtree [s]
  (if (nil? s)
    true
    (do
      (if (false? s)
        false
        (do
          (if (not= (count s) 3)
            false
            (do
              (if-not (zero? (compare (vec (second s)) (vec (nth s 2))))
                false
                (and (symtree (second s)) (symtree (nth s 2)))))))))))

;; #46
(defn p46 [f]
  (fn [a b]
    (f b a)))

;; #44
(defn p44 [n s]
  (let [n* (rem n (count s))]
    (if (pos? n*)
      (concat (drop n* s) (take n* s))
      (concat (take-last (unchecked-negate n*) s) (drop-last (unchecked-negate n*) s))
      )))

(take-nth 2 [1 2 3 4 5 6])
 
(difference '(1 2 3 4 5 6) '(1 3 5))

(partition 3 (apply interleave  (partition 3 (range 9))))

(take-nth 3 (range 9))

(range 9)

;; #43
(defn p43 [s n]
  (let [cnt (/ (count s) n)]
    (partition cnt
               (apply interleave
                      (partition n s)))))

;; W3D3#50
(defn p50 [l]
  (into #{} (vals (group-by type l))))

;; W3D4#67
(defn p67 [n]
  (loop [i 3
         result [2]]
    (if (= (count result) n)
      result
      (do
        (let [res (reduce (fn [a b]
                            (if (zero? (mod a b))
                              (reduced nil)
                              a))
                          i (range 2 i))]
          (recur (inc i)
                 (if (nil? res)
                   result
                   (conj result res))))))))

;; W3D5#77
(defn p77 [l]
  (into #{}
        (filter 
         (fn [s] 
           (not= (count s) 1)) 
         (into #{} 
               (map (fn [a]
                      (reduce 
                       (fn  [b c]
                         (if (zero? (compare 
                                     (str (sort (last b))) 
                                     (str (sort c))))
                           (conj b c)
                           b))
                       #{a}
                       l))
                    l)))))

;; #55
(defn p55 [coll]
  (loop [s (set coll)
           c coll
           result {}]
      (if (empty? s)
        result
        (recur (rest s)
               c
               (conj result {(first s) (count (filter #(= (first s) %) c))})))))

;; #58
(defn p58 [& f]
  (fn [a & b]
    (loop [fs (reverse f)
           b b
           result (if (nil? b) a (cons a b))]
      (if (empty? fs)
        result
        (recur (rest fs)
               nil
               (do
                 (if (nil? b)
                   ((first fs) result)
                   (apply (first fs) result))))))))

;; #59
(defn p59 [& f]
  (fn [& b]
    (loop [fs f
          result []]
      (if (empty? fs)
        result
        (recur (rest fs)
               (conj result (apply (first fs) b)))))))

;; #60
(defn p60 [f a & b]
  (let [l (vec (flatten (conj (vec (conj [] a)) (vec b))))
        res (if (nil? b)
              (reduce f (flatten (vec a)))
              (reduce f a (flatten (vec b))))
        cnt (count l)]
    (loop [i 0
           result []]
      (if (>= i cnt)
        (repeatedly result)
        (recur (inc i)
               (if (coll? res) 
                 (conj result (vec (take (+ i 1) l)))
                 (conj result (apply f (take (+ i 1) l))) ))) )))


;; another working solution
(defn p60
  ([f a]
   (p60 f (first a) (rest a)))
  ([f init [c & cols]]
   (cons init (when c (p60 f (f init c) cols)))))


;; #74
(defn p74 [str]
  (let [perfect-square? (fn [n]
                          (let [f (Math/sqrt n)]
                            (zero? (- f (int f)))))
        str->vec (fn [s]
                   (map #(Integer/parseInt %) (clojure.string/split s #",")))]
    (clojure.string/join "," (filter #(perfect-square? %) (str->vec str)))))

(p74 "4,5,6,7,8,9")

(p74 "15,16,25,36,37")

;; #80
(defn p80 [n] 
  (loop [i 1
         result []]
    (if (>= i n)
      (= (reduce + 0 result) n)
      (recur (inc i)
             (do
               (if (zero? (mod n i))
                 (conj result i)
                 result))))))


