(ns clooj.myrepl
  (:require [clojure.main :as clm])
	(:gen-class))

(defn -main [] (clm/repl :need-prompt (fn [] false) :prompt #(fn [] "")))