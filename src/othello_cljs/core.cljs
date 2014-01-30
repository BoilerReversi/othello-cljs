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
;; split this into modules later mb
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

(def directions '(:n :s :e :w :nw :sw :ne :se))

(def directional-walks
  {:n (partial iterate (fn [[row col]] [(dec row) col]))
   :s (partial iterate (fn [[row col]] [(inc row) col]))
   :e (partial iterate (fn [[row col]] [row (inc col)]))
   :w (partial iterate (fn [[row col]] [row (dec col)]))
   :nw (partial iterate (fn [[row col]] [(dec row) (dec col)]))
   :sw (partial iterate (fn [[row col]] [(inc row) (dec col)]))
   :ne (partial iterate (fn [[row col]] [(dec row) (inc col)]))
   :se (partial iterate (fn [[row col]] [(inc row) (inc col)]))})

;; get-lines returns a map with keys #{:n :s :e :w :nw :sw :ne :se}
;; whose values are lists of the flippable line in that direction
(defn get-lines [board coord]
  (let [in-bounds? (fn [[row col]] (and (< row 7) (< col 7)
                                       (>= row 0) (>= col 0)))
        get-directional-walk (fn [dir coord] ;; WHAT A MESS
                               (take-while 
                                #(not= % :empty)
                                (rest 
                                 (map 
                                  (partial get-in board)
                                  (take-while
                                   in-bounds?
                                   ((directional-walks dir) coord))))))]
    (reduce (fn [m dir] (assoc m dir (get-directional-walk dir coord)))
            {} directions)))

(defn flippable-directions [board player coord]
  (let [sq (get-in board coord)
        flippable? (fn [ls] (and (= (first ls) (flip-player player))
                                 (some #(= player %) (rest ls))))]
    (if (not= :empty sq)
      '()
      (reduce (fn [ls [k v]] (if (flippable? v) (cons k ls) ls)) '() (get-lines board coord)))))


(defn flip-direction [board player coord direction]
  (reduce (fn [brd crd] (flip-square brd crd)) board
          (take-while (fn [crd] (not= player (get-in board crd))) ((directional-walks direction) coord))))

;;returns vector [next-board next-player]
;;if illegal move, [next-board next-player] == [board player]
(defn make-move [board player coord]
  (if (empty? (flippable-directions board player coord))
    [board player]
    [(assoc-in (reduce (fn [brd dir] (flip-direction brd player coord dir))
                       board
                       (flippable-directions board player coord))
               coord player)
     (flip-player player)]))

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
  (go (loop [history (list initial-state)
             player :o]
        (println (first history))
        (set! (.-innerHTML (dom/getElement "to-move")) (str "TO MOVE " player))
        (>! to-board-chan (board->table (first history))) ;; TODO: add legal moves/ai if required
        (let [event (<! event-chan)]
          (match [event]
            [[:move n]] 
            (let [row (quot n 8), col (mod n 8)]
              (let [[next-board next-player] (make-move (first history) player [row col])]
                (if (= next-board (first history))
                  (recur history player)
                  (recur (cons next-board history) next-player))))
            [[:undo]] (if (empty? (rest history))
                        (recur history player)
                        (recur (rest history) (flip-player player)))
            [[:reset]] (recur (list initial-state) :o)
            [[:legal b]] (do (println (str "LEGAL " b)) (recur history player)) ;; TODO: set flag or something
            [[:ai b]] (do (println (str "AI " b)) (recur history player)) ;; TODO: set flag or something
            :else (do (println "ERROR WTF") (recur history player)))))))
