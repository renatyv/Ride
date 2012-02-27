(ns clooj.myns1
  (:import (javax.swing AbstractListModel BorderFactory JDialog
                        JFrame JLabel JList JMenuBar JOptionPane
                        JPanel JScrollPane JSplitPane JTextArea
                        JTextField JTree KeyStroke SpringLayout JTextPane
                        ListSelectionModel
                        UIManager)
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
           (java.io File FileReader StringReader
                    BufferedWriter OutputStreamWriter FileOutputStream))
  (:use [clojure.repl]))

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
          (System/exit 0))))))


(defn create-app []
  (let [frame (JFrame.)
        cp (.getContentPane frame)
        run-result-text (JLabel. "here you will see run results")
        text-area (make-text-area false)
        layout (FlowLayout.)]
    (doto frame
      (.setBounds 25 50 950 700)
      (.setLayout layout)
      (.add text-area)
      (.add run-result-text)
      (.setTitle (str "Title " "title"))
      (.setVisible true))

    (.layoutContainer layout frame)
    (exit-if-closed frame)
    {:frame frame,
     :contentPane cp
     :layout layout
     :text-area text-area
     :run-result-text run-result-text}))

(def app (create-app))

(defn read-string-at [source-text start-line]
     (let [sr (java.io.StringReader. source-text)
           rdr (proxy [clojure.lang.LineNumberingPushbackReader] [sr]
                 (getLineNumber []
                                (+ start-line (proxy-super getLineNumber))))]
       (take-while #(not= % :EOF_REACHED)
                   (repeatedly #(try (do
                                        (hash-map
                                          (.getLineNumber rdr)
                                          (read rdr)))
                                     (catch Exception e :EOF_REACHED))))))

(defn drop-line [text]
  (drop-while
    #(not= % \newline)
    (drop-while
      #(= % \newline)
      text)))

(drop-line 
  (str (println-str "(+ 1 2 3)") (println-str "(* 1 2)")))

(defn map-lines-to-forms [text]
  (letfn [(map-lines-to-forms-l [text line-number]
              (if (empty? text)
                {}
                (if-let [read-result (try (read-string text)
                                          (catch Exception e1 false))]
                        (into 
                          {line-number read-result}
                          (map-lines-to-forms-l 
                                      (drop-line text) (inc line-number)))
                        (map-lines-to-forms-l 
                                      (drop-line text) (inc line-number)))))]
              (map-lines-to-forms-l text 0)))

(map-lines-to-forms 
  (str (println-str "(+ 1 2 3)") (println-str "(* 1 2)")))

(def text-set-agent (agent 0))

(defn text-evaluator [state text]
  (.setText (:run-result-text app)
            (print-str
              (try
                (doall (read-string-at text 0))
                (catch Throwable t (.getMessage t)))
              "|"
              (try 
                (eval (read-string text))
                (catch Throwable t (.getMessage t))))))



(defn create-change-listener []
  (letfn [(update [log-text]
        (println 
          (str
            log-text
            (send-off
              text-set-agent
              text-evaluator
              (.getText (:text-area app))))))]
    (proxy [DocumentListener] []
      (insertUpdate [event] (update "insert"))
      (removeUpdate [event] (update "remove"))
      (changedUpdate [event] (println "change")))))

(.. (:text-area app) (getDocument) (addDocumentListener (create-change-listener)))




