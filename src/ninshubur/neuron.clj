(ns ninshubur.neuron)

(def edge-seq
  (concat
   (for [to [:a :b],
         from [0 1 2 3 4 :a :b]]
     [from to])
   (for [to [:c :d]
         from [:a :b :c :d]]
     [from to])))

(defn translate-vertex [vertex]
  (let [[_ _name tau sigma gain] vertex]
    {:tau tau :sigma sigma :gain gain}))

(defn translate-cluster [cluster]
  (let [[nums vertices] (split-with number? cluster)
        edge-map (zipmap edge-seq nums)
        vertex-map (zipmap [:a :b :c :d]
                           (map translate-vertex vertices))]
    (merge edge-map vertex-map)))

(defn clamp [cur min max]
  (max min (min cur max)))

(defn delta-movement [outputs]
  (let [c (get-in outputs [:c :o])
        d (get-in outputs [:d :o])]
    (-> (- c d) (* 5) (clamp -4 4))))
