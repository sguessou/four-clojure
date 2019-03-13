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
