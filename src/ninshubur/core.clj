(ns ninshubur.core
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp :refer [cl-format]]
            [clojure.tools.cli :refer [cli]]
            [ninshubur.neuron :as nn]
            [ninshubur.term :as t :refer [in-term]]
            [ninshubur.simulate :as sim])
  (:import (java.net Socket))
  (:gen-class))

(defn fmt [out string & args]
  (cl-format *out* (str "out=> " string) args)
  (cl-format out string args))

(defn ert->clj [str]
  (-> str
      (.replace \{ \[)
      (.replace \} \])))

(defn connector [vals]
  (let [lguy (atom nil)]
    (with-open [s (Socket. "localhost" 1055)]
      (let [lcount (atom 0)]
        (with-open [in (io/reader s)
                    out (io/writer s)]
          (fmt out "[~{~A~^, ~}].~%"
               (map (assoc vals :c "cognitive")
                    [:generations :population :selection-type
                     :tournament-size :tournament-luck :scale
                     :mix-type :mix-factor :c :mutation-p :sigma-divisor]))
          (doseq [line (line-seq in)]
            (let [res (read-string (ert->clj line))]
              (swap! lcount inc)
              (reset! lguy (last res))
              (->> (take 4 res)
                   (zipmap [:avg :stddev :max :min])
                   (seq)
                   (cl-format nil "=> ~{~{~s: ~8,5f~}~^, ~}")
                   (println @lcount)))))))
    (println (nn/translate-cluster @lguy))))

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
              :default 100 :parse-fn #(Integer/parseInt %)]
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
             ["--mutation-p" "Mutation probability" :default 5
              :parse-fn #(Integer/parseInt %)]
             ["--sigma-divisor" "Sigma divisor" :default 10.0
              :parse-fn #(Double/parseDouble %)]
             ["-h" "--help" "Show help" :default false :flag :true])]
    (when (or (:help opts) (empty? args))
      (println banner)
      (System/exit 0)))
  #_
  
  (in-term
   (let [state (-> "testcase" slurp read-string sim/init-state)]
     (trampoline #(sim/draw-state state)))))
