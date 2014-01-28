(ns othello-cljs.core
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [cljs.core.match.macros :refer [match]])
  (:require [goog.dom :as dom]
            [goog.events :as events]
            [cljs.core.async :refer [put! chan <!]]
            [cljs.core.async :as async]
            [cljs.core.match]))

(enable-console-print!)
(println "BOILERreversi")

;; othello
(def flip-player {:x :o, :o :x, :empty :empty, :! :!})

(defn flip-square [board [row col]]
  (update-in board [row col] flip-player))

;; Moves are in [row col :player] format
(defn mark-square [board [row col player]]
  (assoc-in board [row col] player))

(def initial-state
  (let [empty-board (vec (map vec (partition 8 (repeat 64 :empty))))
        initial-marks (list [3 3 :x] [3 4 :o]
                            [4 3 :o] [4 4 :x])]
    (reduce mark-square empty-board initial-marks)))
    
(defn board->table [board]
  (let [keyword->text {:x "x", :o "o", :! "!", :empty "_"}]
    (map keyword->text (flatten board))))

;; concurrency helpers
(defn listen [e1 type]
  (let [out (chan)]
    (events/listen e1 type (fn [e] (put! out e)))
    out))

(defn othello-moves-chan []
  (let [out (chan)
        squares (range 0 64)
        elements (doall (map (comp dom/getElement str) squares))
        square-listen (fn [e n]
                        (events/listen e "click" (fn [e] (put! out n))))]
    (doall (map square-listen elements squares))
    out))

;; MAIN
(let [;; DOM elements
      undo-button (dom/getElement "undo")
      legal-button (dom/getElement "legal")
      ai-button (dom/getElement "ai")
      reset-button (dom/getElement "reset")
      ;; raw listeners
      undo-chan (listen undo-button "click")
      legal-chan (listen legal-button "click")
      ai-chan (listen ai-button "click")
      reset-chan (listen reset-button "click")
      ;; board "uart"
      to-board-chan (chan)
      from-board-chan (othello-moves-chan)
      ;; central event channel
      event-chan (chan)
      ]
  ;; Handle input events  
  ;; FROM BOARD
  (go (while true
        (let [m (<! from-board-chan)]
          (>! event-chan [:move m]))))

  ;; UNDO
  (go (while true
        (<! undo-chan)
        (>! event-chan [:undo])))
  ;; LEGAL
  (go (loop [last-checked false]
        (<! legal-chan)
        (let [now-checked (.-checked legal-button)]
          (when (not= now-checked last-checked)
            (>! event-chan [:legal now-checked]))
          (recur now-checked))))
  ;; AI
  (go (loop [last-checked false]
        (<! ai-chan)
        (let [now-checked (.-checked ai-button)]
          (when (not= now-checked last-checked)
            (>! event-chan [:ai now-checked]))
          (recur now-checked))))
  ;; RESET
  (go (while true
        (<! reset-chan)
        (>! event-chan [:reset])))
  
  ;; Physical board output
  ;; Equivalent to micro attached to LEDs
  (go (while true
        (let [state (<! to-board-chan)]
          (doall (map-indexed
                  (fn [i sq]
                    (set! (.-innerHTML (dom/getElement (str i))) sq))
                  state)))))

  ;; CENTRAL EVENT HANDLING
  (go (loop [history (list initial-state)]
        (>! to-board-chan (board->table (first history))) ;; TODO: add legal moves/ai if required
        (let [event (<! event-chan)]
          (match [event]
            [[:move n]] 
            (let [row (quot n 8), col (mod n 8)]
              (recur (if (= (get-in (first history) [row col]) :empty)
                       history
                       (cons (flip-square (first history) [row col]) history)))) ;; !!
            [[:undo]] (recur (if (empty? (rest history))
                               history
                               (rest history)))
            [[:reset]] (recur (list initial-state))
            [[:legal b]] (do (println (str "LEGAL " b)) (recur history)) ;; TODO: set flag or something
            [[:ai b]] (do (println (str "AI " b)) (recur history)) ;; TODO: set flag or something
            :else (do (println "ERROR WTF") (recur history)))))))
