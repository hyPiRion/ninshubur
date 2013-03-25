(ns ninshubur.term
  (:require [lanterna.terminal :as t]
            [ninshubur.vars :as v]))

(def ^:dynamic *t*)
(def ^:dynamic *border-char* \#)
(def ^:dynamic *output-char* \#)
(def ^:dynamic *alert-char* \!)

(defn draw-border []
  (t/set-fg-color *t* :white)
  (t/move-cursor *t* 0 0)
  (dotimes [x (+ v/*area-width* 2)]
    (t/put-character *t* *border-char*))
  (dotimes [y v/*area-height*]
    (t/move-cursor *t* 0 (inc y))
    (t/put-character *t* *border-char*)
    (t/move-cursor *t* (inc v/*area-width*) (inc y))
    (t/put-character *t* *border-char*))
  (t/move-cursor *t* 0 (+ 1 v/*area-height*))
  (dotimes [x (+ v/*area-width* 2)]
    (t/put-character *t* *border-char*)))

(defn draw-object [object]
  (let [{:keys [color char y x length]} object]
    (t/set-fg-color *t* color)
    (dotimes [i length]
      (t/move-cursor *t* (inc (mod (+ x i) v/*area-width*)) (inc y))
      (t/put-character *t* char))))

(def output-color
  {0 :black 1 :green 2 :green 3 :yellow 4 :yellow 5 :red})

(defn y-output-char [val]
  (cond (< val -5) \*
        (< val 0)  \-
        (< val 6)  \#
        :otherwise \!))

(defn y-output-color [val]
  (cond (< val -5) :blue
        (< val 0)  :cyan
        (< val 6)  :magenta
        :otherwise :red))

(def neuron-positions
  {:a {:x (+ v/*area-width* 5)
       :y 5}
   :b {:x (+ v/*area-width* 8)
       :y 5}
   :c {:x (+ v/*area-width* 5)
       :y 15}
   :d {:x (+ v/*area-width* 8)
       :y 15}})

(defn draw-neuron-output [[neuron-name state]]
  (let [rounded-o (-> state :o (* 5) (double) (Math/round) (int))
        rounded-y (-> state :y (* 5) (double) (Math/round) (int))
        abs-y (Math/abs rounded-y)
        {:keys [x y]} (neuron-positions neuron-name)]
    (t/set-fg-color *t* :white)
    (t/move-cursor *t* x (inc y))
    (t/put-string *t* "oy")
    (t/move-cursor *t* x (+ y 2))
    (dotimes [i 2] ;; two or more, use a for
      (t/put-string *t* (name neuron-name)))
    (t/set-fg-color *t* (output-color rounded-o))
    (dotimes [i rounded-o]
      (t/move-cursor *t* x (- y i))
      (t/put-character *t* *output-char*))
    (t/set-fg-color *t* (y-output-color rounded-y))
    (dotimes [i (min abs-y 6)]
      (t/move-cursor *t* (inc x) (- y i))
      (t/put-character *t* (y-output-char rounded-y)))))

(defn draw-simulation [objects neuron-map]
  (t/clear *t*)
  (draw-border)
  (doseq [obj objects]
    (draw-object obj))
  (doseq [neuron neuron-map]
    (draw-neuron-output neuron))
  (t/move-cursor *t* 0 0))

(defn get-key []
  (t/get-key-blocking *t*))

(defmacro in-term [& body]
  `(binding [*t* (t/get-terminal v/*tty-type*)]
     (t/in-terminal *t* ~@body)))
