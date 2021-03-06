(ns hypercrud.util.core
    (:require [clojure.string :as string]
    #?(:clj [clojure.pprint :as pprint]
       :cljs [cljs.pprint :as pprint])))


(defn map-values [f m]
  (->> (map (juxt key (comp f val)) m)
       (into {})))

(defn map-keys [f m]
  (->> (map (juxt (comp f key) val) m)
       (into {})))

(defn group-by-assume-unique [f xs]
  (->> xs
       (map (juxt f identity))
       (into {})))

(defn update-existing [m k f & args]
  (if (get m k)
    (apply update m k f args)
    m))

(defn parse-query-element [q query-element]
  ; return value is symbols, not strings
  (let [last-kw (atom nil)
        f (fn [x]
            (if (keyword? x) (reset! last-kw x))
            @last-kw)]
    (->> (partition-by f q)
         (filter #(= query-element (first %)))
         first
         (drop 1))))

(defn transpose "Define transpose empty matrix to return the matrix unchanged - this is not math"
  [matrix]
  (if (seq matrix) (apply mapv vector matrix)
                   matrix))

(defn zip [as bs]
  ;(transpose [as bs])
  (map vector as bs))

(defn pad [n zero coll]
  (take n (concat coll (repeat zero))))

(defn map-pad [zero]
  (fn [f & cols]
    (let [n (apply max (map count cols))
          cols (map #(pad n zero %) cols)]
      (apply map f cols))))

(comment
  (map + [1 1 1] [1 1 1 1])                                 ;=> (2 2 2)
  ((map-pad 0) + [1 1 1] [1 1 1 1])                         ;=> (2 2 2 1)
  )

(defn pprint-str [v & [columns]]
  (string/trimr
    (binding [pprint/*print-right-margin* (or columns pprint/*print-right-margin*)]
      (with-out-str (pprint/pprint v)))))

(defn fallback [p v not-found]
  (if-not (p v) v not-found))

(defn tee [g f!] (fn [v] (f! v) (g v)))

(defn kwargs
  "arg format is kwargs first; trailing non-kw args are nil key
      [:a 1 :b 2 'a 'b 'c] => {nil (a b c), :b 2, :a 1}"
  [as]
  (let [[kwargs args] (split-with (comp keyword? first) (partition-all 2 as))]
    (-> (apply hash-map (flatten kwargs))
        (assoc nil (flatten args)))))

(defn truncate
  [s n]
  (subs s 0 (min (count s) n)))

(defn abs-normalized [x]
  #?(:clj  (if x (Math/abs x) 0)
     :cljs (js/Math.abs x)))
