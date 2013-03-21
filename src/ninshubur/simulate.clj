(ns ninshubur.simulate
  (:require [ninshubur.term :as t :refer [*t*]]
            [lanterna.terminal :as tty]
            [ninshubur.vars :as v]))

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
          :hist [{:tracker (init-tracker)
                  :block (make-block)
                  :a {:o 0 :y 0}
                  :b {:o 0 :y 0}
                  :c {:o 0 :y 0}
                  :d {:o 0 :y 0}}]}))

(defn start-term [state]
  (let [time (:time state)
        block (get-in state [:hist time :block])
        tracker (get-in state [:hist time :tracker])]
    (t/draw-simulation #{block tracker})
    (while (not= \q (t/get-key)))))
