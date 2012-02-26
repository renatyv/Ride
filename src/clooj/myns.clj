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

(def text-set-agent (agent 0))

(defn text-evaluator [state text]
  (.setText (:run-result-text app)
            (str (try 
                   (eval (read-string text))
                   (catch Throwable t (.getMessage t))))))

(defn create-change-listener []
  (proxy [DocumentListener] []
    (insertUpdate [event]
      (println 
        (str
          "insert"
          (send-off
            text-set-agent
            text-evaluator
            (.getText (:text-area app))))))
    (removeUpdate [event]
      (println "remove"))
    (changedUpdate [event]
      (println "change"))))

(.. (:text-area app) (getDocument) (addDocumentListener (create-change-listener)))




