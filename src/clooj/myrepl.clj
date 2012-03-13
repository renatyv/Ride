(ns clooj.myrepl
	(:gen-class))

(defn -main [arg]
  (println 
   (try
     (eval (read))
     (catch Exception e (str \" (.getMessage e) \"))))
  (recur arg))