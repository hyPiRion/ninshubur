(ns ninshubur.main
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp :refer [cl-format]]
            [clojure.tools.cli :refer [cli]]
            [ninshubur.neuron :as nn]
            [ninshubur.term :as t :refer [in-term]]
            [ninshubur.simulate :as sim]
            [ninshubur.vars :as v]
            [ninshubur.utils :as utils])
  (:import (java.net Socket))
  (:gen-class))

(defn fmt [out string & args]
  (apply cl-format *out* string args)
  (apply cl-format out string args))

(defn ert->clj [str]
  (-> str
      (.replace \{ \[)
      (.replace \} \])))

(defmulti pick-best
  (fn [old state new] (:sim-type state)))

(defmethod pick-best "random" [_ _ new]
  new)

(defmethod pick-best "brute" [old _ new]
  (max-key :fitness old new))

(defn connector [vals]
  (let [bguy (atom {:fitness -1})
        history (atom [])]
    (dotimes [iter (:repeat vals)]
      (with-open [s (Socket. (:hostname vals) (:port vals))]
        (cl-format true "Run ~r:~%" (inc iter))
        (let [lcount (atom 0)]
          (with-open [in (io/reader s)
                      out (io/writer s)]
            (fmt out "[~{~A~^, ~}].~%"
                 (map (assoc vals :c "cognitive")
                      [:generations :population :type
                       :tournament-size :tournament-luck :scale
                       :mix-type :mix-factor :c :mutation-p :sigma-divisor
                       :crossover-rate :fitness-fn :sim-type]))
            (swap! history assoc iter [])
            (doseq [line (line-seq in)]
              (let [res (read-string (ert->clj line))
                    ptype {:fitness (nth res 2)
                           :gene (nth res 4)}]
                (swap! lcount inc)
                (swap! bguy pick-best vals ptype)
                ;; update a graph plotting tool here.
                (swap! history update-in [iter] conj (vec (take 4 res)))
                (->> (take 4 res)
                     (zipmap [:avg :stddev :max :min])
                     (seq)
                     (mapv (fn [[k v]] [(-> k name symbol) v]))
                     (cl-format true "~4,'0d => ~{~{~s: ~9,5f~}~^, ~}~%"
                                @lcount))))))))
    (when-let [out (:outfile vals)]
      (spit out
            (with-out-str
              (->> @bguy :gene nn/translate-cluster pp/pprint))))
    (when-let [afile (:analyze vals)]
      (->> (utils/transform-3d @history)
           (cl-format nil "~{~{~f~^ ~}~%~}")
           (spit afile)))
    (when (:sim vals)
      (let [state (-> @bguy :gene nn/translate-cluster sim/init-state)]
        (in-term
         (trampoline #(sim/draw-state state)))))))

(defn -main
  "Basic connector to Genetica."
  [& args]
  (let [[opts args banner]
        (cli args
             ["--port" "Genetica's port" :default 1055
              :parse-fn #(Integer/parseInt %)]
             ["--hostname" "Genetica's hostname" :default "localhost"]
             ["-g" "--generations" "Generation count"
              :default 100 :parse-fn #(Integer/parseInt %)]
             ["-p" "--population" "The population size"
              :default 200 :parse-fn #(Integer/parseInt %)]
             ["-t" "--type" "Selection type"
              :default "roulette"]
             ["-T" "--tournament-size" "Tournament size"
              :default 40 :parse-fn #(Integer/parseInt %)]
             ["--tournament-luck" "Tournament luck"
              :default 0.3 :parse-fn #(Double/parseDouble %)]
             ["-s" "--scale" "Scale type"
              :default "sigma"]
             ["-m" "--mix-type" "Mix type" :default "generational_mixing"]
             ["-M" "--mix-factor" "Mix factor"
              :default 100 :parse-fn #(Integer/parseInt %)]
             ["--mutation-p" "Mutation probability" :default 0.5
              :parse-fn #(Double/parseDouble %)]
             ["--sigma-divisor" "Sigma divisor" :default 10.0
              :parse-fn #(Double/parseDouble %)]
             ["--crossover-rate" "Crossover rate" :default 0.9
              :parse-fn #(Double/parseDouble %)]
             ["-f" "--fitness-fn" "Fitness function"
              :default "sum"]
             ["-s" "--sim-type" "Simulation type"
              :default "random"]
             ["-o" "--outfile" "Save last genotype to file"
              :default nil]
             ["-i" "--rerun" "Rerun genotype by file"
              :default nil]
             ["--repeat" "# of simulations"
              :default 1 :parse-fn #(Integer/parseInt %)]
             ["--analyze" "Analyze to file"
              :default nil]
             ["--tracker-max-speed" "Max tracker speed" :default 4
              :parse-fn #(Integer/parseInt %)]
             ["--swing" "Use swing for visualization."
              :default false :flag true]
             ["--hidden-layer" "Use hidden layer" :default true
              :parse-fn #(Boolean/parseBoolean %)]
             ["--sim" "Show simulation" :default true :flag true]
             ["-h" "--help" "Show help" :default false :flag :true])]
    ;; Setup bindings here
    (binding [v/*tracker-max-speed* (:tracker-max-speed opts)
              v/*tty-type* (if (:swing opts) :swing :text)
              v/*hidden-layer?* (:hidden-layer opts)]
      (when (:help opts)
        (println banner)
        (System/exit 0))
      (when (:rerun opts)
        (let [state (-> opts :rerun slurp read-string sim/init-state)]
          (in-term
           (trampoline #(sim/draw-state state)))
          (System/exit 0)))
      (connector opts))))
