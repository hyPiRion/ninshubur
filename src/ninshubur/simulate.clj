(ns ninshubur.simulate
  (:require [ninshubur.term :as t :refer [*t*]]
            [lanterna.terminal :as tty]
            [ninshubur.vars :as v]
            [clojure.pprint :as pp]))


;;; Initialization

(def empty-vertices
  {:a {:o 0 :y 0}
   :b {:o 0 :y 0}
   :c {:o 0 :y 0}
   :d {:o 0 :y 0}})

(defn make-block []
  (let [len (inc (rand-int v/*block-max-size*))
        pos (rand-int v/*area-width*)
        catch? (< len v/*tracker-size*)]
    {:length len, :x pos, :y 0,
     :color (if catch? :green :red), :char (if catch? \o \O)}))

(defn init-tracker []
  {:length v/*tracker-size*, :y (dec v/*area-height*)
   :x 0, :color :blue, :char \-})

(defn init-state [neuron-cluster]
  (merge neuron-cluster
         {:time 0
          :hist [(merge {:tracker (init-tracker)
                         :block (make-block)}
                        empty-vertices)]}))

;;; Updating state

(defn edge-from [x]
  (cond (#{:a :b} x) (list* :a :b (range v/*tracker-size*))
        (#{:c :d} x) (list :a :b :c :d)))

(defn senses [{l :length, pos :x} at]
  (let [nat (mod at v/*area-width*)]
    (->> (range pos (+ pos l))
         (mapv #(mod % v/*area-width*))
         (some #{nat})
         (boolean))))

(defn sense [prev]
  (let [block (:block prev)
        pos (get-in prev [:tracker :x])]
    (mapv (partial senses block)
          (range pos (+ pos v/*tracker-size*)))))

(defn new-vstate [state senses node-name]
  (let [t (:time state)
        edges (for [f (edge-from node-name)]
                [f node-name])
        e+w (mapv (juxt identity state) edges)
        sum (reduce + (for [[[from to] w] e+w]
                        (if (integer? from)
                          (if (senses from) w 0)
                          (* w (get-in state [:hist t from :o])))))
        {o- :o, y- :y} (get-in state [:hist t node-name])
        {tau :tau, sigma :sigma, g :gain} (get state node-name)
        dy (double (/ (- (+ sum sigma) y-) tau))
        y (+ y- dy)
        o (/ (+ 1.0 (Math/exp (- (* g y)))))]
    (assoc-in state [:hist t node-name]
              {:o o, :y y})))

(defn clamp [cur min- max-]
  (max min- (min cur max-)))

(defn actuate [state] ;; returns new position of tracker
  (let [t (:time state)
        c (get-in state [:hist t (if v/*hidden-layer?* :c :a) :o])
        d (get-in state [:hist t (if v/*hidden-layer?* :d :b) :o])
        old-pos (get-in state [:hist t :tracker :x])]
    (-> (- d c) (* (inc v/*tracker-max-speed*)) (double) (Math/round) (int)
        (clamp (- v/*tracker-max-speed*) v/*tracker-max-speed*)
        (+ old-pos) (mod v/*area-width*))))

(defn dup-last [vec]
  (conj vec (peek vec)))

(defn next-step [state]
  (let [t (:time state)
        sensed (sense (get-in state [:hist t]))
        state (-> (update-in state [:hist] dup-last)
                  (update-in [:time] inc)) ;; replicate previous, update time
        state (reduce (fn [s n] (new-vstate s sensed n))
                      state [:a :b :c :d])
        tracker-pos (actuate state)
        t (inc t)]
    (-> state
        (update-in [:hist t :block :y] inc)
        (assoc-in [:hist t :tracker :x] tracker-pos))))

(defn reset-state [prev]
  (-> prev
      (merge empty-vertices)
      (assoc :block (make-block))))

(defn new-state [state]
  (let [t (:time state)
        prev (get-in state [:hist t])
        {y :y :as block} (:block prev)]
    (if (== y (dec v/*area-height*))
      (update-in state [:hist] conj (reset-state prev))
      (next-step state))))

;; Visualisation part.

(declare inc-state dec-state)

(defn draw-state [state]
  (let [time (:time state)
        block (get-in state [:hist time :block])
        tracker (get-in state [:hist time :tracker])
        neurons (-> state
                    (get-in [:hist time])
                    (select-keys [:a :b :c :d]))]
    (t/draw-simulation [tracker block] neurons)
    (case (t/get-key)
      \q state
      :right #(inc-state state)
      :left #(dec-state state)
      #(draw-state state))))

(defn inc-state [state]
  (let [t (min (inc (:time state))
               (* v/*nof-blocks* v/*area-height*))]
    (if-not (contains? (:hist state) t)
      (let [nstate (new-state state)]
        #(draw-state nstate))
      #(draw-state (assoc state :time t)))))

(defn dec-state [state]
  (let [t (max (dec (:time state)) 0)
        nstate (assoc state :time t)]
    #(draw-state nstate)))
