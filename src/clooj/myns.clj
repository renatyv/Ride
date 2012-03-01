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
  (:use [clojure.repl]
        [clooj.utils])
  (:require [clojure.string :as cstr]))

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

(defn drop-line [text]
  (apply str
    (let [[x & xs :as all] (drop-while #(not= \newline %) text)]
      (if (empty? all)
        all
        xs))))

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
                      (eval (get lines-forms-map %))
                      \newline)
                    \newline)
              (range (inc last-line-number)))))))

(defn getSource [] 
  (.getText (:source-text-area app)))

(defn eval-text [text]
  (eval-align-forms 
    (map-lines-to-forms text)))

(map-lines-to-forms 
  (str (println-str "(+ 1 2 3)") (println-str "(* 1 2)")))

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




