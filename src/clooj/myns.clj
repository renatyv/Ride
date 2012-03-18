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
                    StringReader PushbackReader
                    EOFException)
           (clojure.lang LispReader$ReaderException)
           (clojure.lang LineNumberingPushbackReader)
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
        [clooj.utils])
  (:require [clojure.string :as cstr]
            [clojure.java.io :as io]
            [clojure.stacktrace :as strace]))

(defn my-outside-repl []
  (let [java (str (System/getProperty "java.home")
                  File/separator "bin" File/separator "java")
        project-path "."
        builder (ProcessBuilder.
                  [java "-cp" "./classes:./lib/clojure-1.3.0.jar" "clooj.myrepl"])]
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

(def repl (atom (my-outside-repl)))

(defn restart-my-repl []
  (let [oldRepl @repl]
    (do
      (println "restart repl")
      (swap! repl (fn [x] (my-outside-repl)))
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

;TODO support reading multiple forms from output
(defn read-from-outside []
  (let [f (future (read (:input-reader @repl)))]
    (let[result (deref f 5000 :time-out)]
      (if-not (= result :time-out)
        result
        (do
          (future-cancel f)
          (throw (Exception. "read timeout")))))))


(defn eval-outside [form]
  (if (string? form) 
    form
    (let [text-to-eval (str form)]
      (try
          (do 
            ; push current form
            (.println (:input-writer @repl) text-to-eval)
            ;read result
            (read-from-outside))
          (catch Exception e1 
            (do 
              (restart-my-repl)
              (.getMessage e1)))))))

(defn drop-line [text]
  (apply str
    (let [[x & xs :as all] (drop-while #(not= \newline %) text)]
      (if (empty? all) '() xs))))

(defn is-first-line-empty? [text]
  (let [lines (cstr/split-lines text)]
    (or 
      (empty? lines)
      (cstr/blank? (first lines)))))

(defn map-lines-to-forms2 [text]
  (let [r (LineNumberingPushbackReader.
              (StringReader. text))]
    (letfn [(read-next []
              (let [
                read-result
                (do
                  (try
                    (read r false :eof)
                    (catch Exception e1 (.getMessage e1))))]
                (if (= read-result :eof) 
                  (force [(.getLineNumber r) "EOF" true])
                  (force [(.getLineNumber r) read-result false]))))]
      (loop [[line-number current-result eof-error?] (read-next)
              resulting-map {}]
        (if eof-error?
          resulting-map
          (recur
            (read-next)
            (into 
              resulting-map
              (hash-map line-number current-result))))))))

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
      (catch Exception e1 
        (do 
          (strace/print-stack-trace e1)
          (.getMessage e1)))))
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