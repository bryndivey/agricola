(ns ^:shared agricola-client.game
    (:require [agricola-client.actions :as actions]
              [agricola-client.utils :as utils]))





(defn round-done? [game]
  (let [remaining (utils/remaining-moves game)]
    (not (some pos? (vals remaining)))))

(defn harvest-round? [game]
  ((set (:round-lengths game)) (:round game)))

(defn next-player-harvest [game]
  (let [harvest-moves (filter #(= :harvest (:type %)) (utils/round-moves game))
        starting-player (utils/get-starting-player game)
        players (utils/rotate-while (comp not (:id starting-player)) (:player-order game))
        played-harvest (set (map :player harvest-moves))
        with-moves (filter (complement played-harvest) players)]
    (first with-moves)))

(defn next-player-game [game]
  (let [r-moves (utils/round-moves game)
        starting-player (utils/get-starting-player game)
        players (utils/rotate-while (comp not (:id starting-player)) (:player-order game))
        remaining (utils/remaining-moves game)]
    (if (empty? r-moves)
      (first players)
      (let [last-player (:player (last (:moves game)))
            rotated (utils/rotate 1 (utils/rotate-while (partial not= last-player) players))
            with-moves (filter #(pos? (remaining %)) rotated)]
        (first with-moves)))))

(defn next-move [game]
  (let [done (round-done? game)
        harvest-round (harvest-round? game)
        move (cond
              (and done harvest-round) {:type :harvest :player (next-player-harvest game)}
              (not done) {:type :game :player (next-player-game game)})]
    (if (:player move)
      move
      {:type :tick})))




(defn slot-tick [slot]
  (let [slot (assoc slot :performed false)
        fn (actions/get-action-fn (:action slot) :tick)]
    (if fn
      (fn slot)
      slot)))

(defn game-tick [game]
  (let [g (-> game
              (assoc :slots (zipmap (keys (:slots game))
                                    (map slot-tick (vals (:slots game)))))
              (update-in [:round] inc))]
    (assoc g :next-move (next-move g))))




(defn game-loop 
  "Takes an action and game, returns the game and next required move"
  [game move]
  (let [game (actions/perform-move game move)
        next-move (try (next-move game)
                       (catch Exception e {:type :tick}))]
    (assoc game :next-move next-move)))
