(ns clooj.MyRepl
	(:gen-class))

(defn -main []
  (println 
   (try
     (eval (read))
     (catch Exception e (.getMessage e))))
  (recur))