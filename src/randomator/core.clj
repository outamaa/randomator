(ns randomator.core
  (:require [clj-http.client :as http]))

(defn parse-board [string]
  (loop [board {} x 0 y 0 data (seq string)]
    (cond
      (empty? data)
      board
      (= (first data) \newline)
      (recur board 0 (+ y 1) (rest data))

      (= (first data) \.)
      (recur
       (assoc board [x y] :empty)
       (+ x 1) y (rest data))

      (= (first data) \space)
      (recur board x y (rest data))

      (= (first data) \$)
      (recur (assoc board [x y] :item) (+ x 1) y (rest data))

      (= (first data) \@)
      (recur (assoc board [x y] :me) (+ x 1) y (rest data))

      (= (first data) \e)
      (recur (assoc board [x y] :enemy) (+ x 1) y (rest data))

      (= (first data) \#)
      (recur board (+ x 1) y (rest data))

      (= (first data) \:)
      (recur (assoc board [x y] :empty) (+ x 1) y (rest data))

      (re-find #"[A-Z]" (str (first data)))
      (recur (assoc board [x y] :player) (+ x 1) y (rest data))

      :else
      (do
        (println "BAD CHAR:" (first data))
        (recur board x y (rest data))))))

(defn add-player [{:keys [url] :as state} name pass]
  (let [resp (try (http/get (str url "/api/add-player?name=" name "&pass=" pass))
                  (catch Exception e
                    (println "failed to add player " name)))]
    (if resp
      (merge state
             {:name name
              :pass pass
              :board (parse-board (:body resp))})
      state)))

(defn next-board [{:keys [url name] :as state}]
  (let [resp (http/get (str url "/api/next-board?name=" name))]
    (assoc state :board (parse-board (:body resp)))))

(defn act [{:keys [url name pass] :as state} action target]
  {:pre [name pass]}
  (let [resp (try (http/get (str url "/api/act?name=" name "&pass=" pass "&action=" action "&target=" target))
                  (catch Exception e
                    (println "action failed: " action " " target)))]
    state))

(defn my-coordinates [board]
  (ffirst (filter #(= (second %) :me) (seq board))))

(defn neighbor-coords [[x y :as coords]]
  (when coords [[x (inc y)] [x (dec y)] [(inc x) y] [(dec x) y]]))

(defn neighbors [board coords]
  (select-keys board (neighbor-coords coords)))

(defn valid-moves [coord-map]
  (into {} (filter (fn [[_ type]]
                     (or (= type :empty)
                         (= type :item)))
                   coord-map)))
(defn random-element [coll]
  (when (first coll)
    (rand-nth coll)))

(defn direction [my-coords new-coords]
  ({[1 0]  "east"
     [-1 0] "west"
     [0 1]  "south"
     [0 -1] "north"} (mapv - new-coords my-coords)))

(defn make-valid-move [{:keys [board] :as state}]
  (let [my-coords (my-coordinates board)
        direction (->> (neighbors board my-coords)
                       (valid-moves)
                       (map first)
                       (random-element)
                       (direction my-coords))]
    (if direction
      (do (println "move" direction)
          (act state "move" direction))
      (println "no valid move direction, or I am dead"))))

(defn -main [& [name pass]]
  {:pre [name pass]}
  (let [initial-state (add-player {:url "http://localhost:8080"} name pass)]
    (if (:name initial-state)
      (loop [state initial-state]
        (recur (-> (next-board initial-state)
                   (make-valid-move)))))
    (println "could not add player" name)))
