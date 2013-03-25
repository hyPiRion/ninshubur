(ns ninshubur.utils)

(defn avg [xs]
  (/ (reduce + xs)
     (count xs)))

(defn std-dev
  ([xs]
     (std-dev xs (avg xs)))
  ([xs avg]
      (let [sums (reduce (fn [acc v]
                           (let [d (- v avg)]
                             (+ acc (* d d))))
                         xs)]
        (Math/sqrt (/ sums
                      (-> xs count dec))))))

(defn transform-3d [data]
  (->> data
       (apply map list)
       (map (partial apply map list))
       (map #(mapcat (juxt avg std-dev) %))))
