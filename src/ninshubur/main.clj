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

(defn connector [args]
  (let [bguy (atom {:fitness -1})
        history (atom [])]
    (dotimes [iter (:repeat args)]
      (with-open [s (Socket. (:hostname args) (:port args))]
        (cl-format true "Run ~r:~%" (inc iter))
        (let [lcount (atom 0)]
          (with-open [in (io/reader s)
                      out (io/writer s)]
            (fmt out "[~{~A~^, ~}].~%"
                 (map (assoc args :c "cognitive"
                             :fitness-fn "none")
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
                (when (identical? (swap! bguy pick-best args ptype) @bguy)
                  ;; You know, this is always true.
                  (when-let [out (:outfile args)]
                    (spit out
                          (with-out-str
                            (->> @bguy :gene nn/translate-cluster pp/pprint)))))
                ;; update a graph plotting tool here.
                (swap! history update-in [iter] conj (vec (take 4 res)))
                (->> (take 4 res)
                     (zipmap [:avg :stddev :max :min])
                     (seq)
                     (mapv (fn [[k v]] [(-> k name symbol) v]))
                     (cl-format true "~4,'0d => ~{~{~s: ~9,5f~}~^, ~}~%"
                                @lcount))))))))
    (when-let [afile (:analyze args)]
      (->> (utils/transform-3d @history)
           (cl-format nil "~{~{~f~^ ~}~%~}")
           (spit afile)))
    (when (:sim args)
      (let [state (-> @bguy :gene nn/translate-cluster sim/init-state)]
        (in-term
         (trampoline #(sim/draw-state state)))))))

(def postlude
"
In-depth explanations:

Generations specifies how many generations to run for.

Population specifies how many phenotypes there will be in a single
population.

Selection type can either be \"roulette\" or \"tournament\", and is by default
\"roulette\".

Tournament size decides how many there should be in a tournament, if tournament
selection is chosen.

Scale type can be either \"sigma\", \"boltzmann\", \"fitness\" or \"rank\", and
is only activated when there is a roulette selection.

Mix type can be either \"full_replacement\", \"overproduction\" or
\"generational_mixing\", and this specifies what kind of population mixing
scheme is used.

Mix factor does nothing when full replacement is chosen as mix type. When
overproduction is selected, this specifies how many *additional* children which
will be produced. When generational mixing is selected, it specifies how many
parents will remain in the new population.

Mutation probability specifies the probability that a genome will get mutated
when generated.

Sigma divisor specifies *how much* a mutation will change. The higher, the lower
change.

Crossover rate specifies the probability that a genome parameter is taken from
the other genome when performing crossover.

Simulation type specifies what kind of simulation should be done: \"rand\" is
slow and unpredictable, whereas \"brute\" will speed up the fitness and give
consistent results.

Outfile specifies the filename where the best genotype is saved. When omitted,
the genotype is not saved.

Rerun specifies the filename of the genotype to rerun. When not specified, will
not rerun and rather attempty to connect to Genetica instead.

Analyze specifies where to spit out an analysis of the runs. Requires two or
more runs, will otherwise crash the system. When omitted, will not do an
analysis.

Tracker max speed specifies the maximal speed of the tracker.

Swing is a flag, which sets up a Swing GUI terminal instead of running inside a
normal terminal. Use this only if you have issues with printing in the console.

Hidden layer specifies whether the hidden layer of the CTRNN should be used or
not.

Sim specifies whether one should simulate or not. When set, will simulate the
best phenotype after all runs have finished.

Help shows this help output.
")

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
;             ["-f" "--fitness-fn" "Fitness function"
;              :default "sum"]
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
      (when (or (:help opts) (seq args))
        (println banner postlude)
        (System/exit 0))
      (when (:rerun opts)
        (let [state (-> opts :rerun slurp read-string sim/init-state)]
          (in-term
           (trampoline #(sim/draw-state state)))
          (System/exit 0)))
      (connector opts))))
