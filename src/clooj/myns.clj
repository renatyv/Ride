(ns clooj.myns1
  (:import (javax.swing AbstractListModel BorderFactory JDialog
                        JFrame JLabel JList JMenuBar JOptionPane
                        JPanel JScrollPane JSplitPane JTextArea
                        JTextField JTree KeyStroke SpringLayout JTextPane
                        ListSelectionModel
                        UIManager)
           (java.io BufferedReader BufferedWriter
                    InputStreamReader
                    File PipedReader PipedWriter PrintWriter Writer
                    StringReader PushbackReader)
           (javax.swing.event TreeSelectionListener
                              TreeExpansionListener
                              DocumentListener
                              DocumentEvent)
           (javax.swing.tree DefaultMutableTreeNode DefaultTreeModel
                             TreePath TreeSelectionModel)
           (java.awt Insets Rectangle Window FlowLayout)
           (java.awt.event AWTEventListener FocusAdapter MouseAdapter
                           WindowAdapter KeyAdapter)
           (java.awt AWTEvent Color Font GridLayout Toolkit)
           (java.net URL)
           (java.io File FileReader StringReader Reader BufferedReader
                    PushbackReader InputStreamReader
                    BufferedWriter OutputStreamWriter FileOutputStream))
  (:use [clojure.repl]
        [clooj.utils]
        [clooj.repl])
  (:require [clojure.string :as cstr]
            [clojure.java.io :as io]))

(defn my-outside-repl
  "This function creates an outside process with a clojure repl."
  [project-path]
  (let [java (str (System/getProperty "java.home")
                  File/separator "bin" File/separator "java")
        builder (ProcessBuilder.
                  [java "-cp" "./classes:./lib/clojure-1.3.0.jar" "clooj.myrepl" "randomarg"])]
    (.redirectErrorStream builder true)
    (.directory builder (File. (or project-path ".")))
    (let [proc (.start builder)
          input-writer  (-> proc .getOutputStream (PrintWriter. true))
          input-reader (PushbackReader. (InputStreamReader. (.getInputStream proc)))
          repl {:input-writer input-writer
                :input-reader input-reader
                :project-path project-path
                :thread nil
                :proc proc}
          ]
      repl)))

(def repl (atom (my-outside-repl ".")))

(defn restart-my-repl []
  (let [oldRepl @repl]
    (do
      (println "restart repl")
      (swap! repl my-outside-repl)
      (.destroy (:proc oldRepl)))))

(defn stop-repl []
  (do (.destroy (:proc @repl))))

(defn make-text-area [wrap]
  (doto (proxy [JTextPane] []
          (getScrollableTracksViewportWidth []
            (if-not wrap
              (if-let [parent (.getParent this)]
                (<= (. this getWidth)
                    (. parent getWidth))
                false)
              true)))))

(def embedded (atom true))

(defn exit-if-closed [^java.awt.Window f]
  (when-not @embedded
    (.addWindowListener f
      (proxy [WindowAdapter] []
        (windowClosing [_]
          (stop-repl)
          (System/exit 0))))))


(defn create-app []
  (let [frame (JFrame.)
        cp (.getContentPane frame)
        run-result-text (make-text-area false)
        source-text-area (make-text-area false)
        divider-size 0
        resize-weight 0.5]
    (doto frame
      (.setBounds 25 50 950 700)
      (.setTitle (str "Title " "title"))
      (.setVisible true))
    (doto cp
      (.add (make-split-pane 
        source-text-area 
        run-result-text 
        true 
        divider-size 
        resize-weight)))
    (exit-if-closed frame)
    {:frame frame,
     :contentPane cp
     :source-text-area source-text-area
     :run-result-text run-result-text}))

(def app (create-app))



(defn read-outside []
  (let [f (future (read (:input-reader @repl)))]
    (if-let[result (deref f 100 false)]
      result
      (do
        (future-cancel f)
        (restart-my-repl)
        nil))))


(defn eval-outside [form]
  (let [text-to-eval (str form)]
        (do 
          ; push current form
          (.println (:input-writer @repl) text-to-eval)
          ;read result
          (read-outside))))

(defn drop-line [text]
  (apply str
    (let [[x & xs :as all] (drop-while #(not= \newline %) text)]
      (if (empty? all) '() xs))))

(defn is-first-line-empty? [text]
  (let [lines (cstr/split-lines text)]
    (or 
      (empty? lines)
      (cstr/blank? (first lines)))))

(defn map-lines-to-forms [text]
  (letfn [(map-lines-to-forms-l [text line-number]
              (if (cstr/blank? text)
                {}
                (if (is-first-line-empty? text)
                  (map-lines-to-forms-l (drop-line text) (inc line-number))
                  (if-let [read-result (try (read-string text)
                                            (catch Exception e1 false))]
                          (into 
                            {line-number read-result}
                            (map-lines-to-forms-l 
                                        (drop-line text) (inc line-number)))
                          (map-lines-to-forms-l 
                                        (drop-line text) (inc line-number))))))]
              (map-lines-to-forms-l text 0)))

(defn eval-align-forms [lines-forms-map]
  (if (empty? lines-forms-map)
    ""
    (let[last-line-number (apply max (keys lines-forms-map))]
      (apply str
            (map 
              #(if (get lines-forms-map %)
                    (str 
                      (eval-outside (get lines-forms-map %))
                      \newline)
                    \newline)
              (range (inc last-line-number)))))))

(defn getSource [] 
  (.getText (:source-text-area app)))

(defn eval-text [text]
  (eval-align-forms 
    (map-lines-to-forms text)))

(def text-set-agent (agent 0))

(defn text-evaluator [state text]
  (.setText 
    (:run-result-text app)
    (try 
      (eval-text text)
      (catch Exception e1 (.toString e1))))
  (inc state))

(defn create-change-listener []
  (letfn [(update [log-text]
        (println 
          (str
            log-text
            (send-off
              text-set-agent
              text-evaluator
              (.getText (:source-text-area app))))))]
    (proxy [DocumentListener] []
      (insertUpdate [event] (update "insert"))
      (removeUpdate [event] (update "remove"))
      (changedUpdate [event] (println "change")))))



(.. (:source-text-area app) (getDocument) (addDocumentListener (create-change-listener)))