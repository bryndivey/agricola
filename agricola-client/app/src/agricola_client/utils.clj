(ns ^:shared agricola-client.utils)

;; players
(defn get-player [game player]
  (get-in game [:players player]))

(defn set-player [game name player]
  (assoc-in game [:players name] player))


;; huts

(defn add-hut
  ([player space type]
     (assoc-in player [:board space :hut] type))
  ([game player space type]
     (assoc-in game [:players player :board space :hut] type)))

(defn count-huts [player]
  (count (filter :hut (vals (:board player)))))

(defn hut-type [player]
  (:hut (first (filter :hut (vals (:board player))))))


;; stables

(defn add-stable [player space]
  (assoc-in player [:board space :stable] true))

(defn count-stables [player]
  (count (filter :stable (vals (:board player)))))

;; sowing fields

(defn sow-field [player space crop]
  (-> player 
      (assoc-in [:board space :resource :type] crop)
      (assoc-in [:board space :resource :number] (cond
                                                  (= crop :grain) 3
                                                  (= crop :vegetable) 2
                                                  :else (throw (Exception. "bad crop!"))))))

;; fences

(defn build-fence [player space side]
  (assoc-in player [:board space :fences side] true))


;; resource manipulation

(defn has-at-least?
  ([game player r-map]
     (has-at-least? (-> game :players player) r-map))
  ([player r-map]
     (let [r2 (merge-with - (:resources player) (select-keys r-map (keys (:resources player))))]
       r2
       (not-any? neg? (vals r2)))))

(defn- munge-resources
  "Modify the player's resources dict with an op, only modifying resources that exist in the player's map"
  ([op player r-map]
     (assoc player :resources
            (merge-with op
                        (:resources player)
                        (select-keys r-map (keys (:resources player))))))
  ([op game player r-map]
     (let [rpath [:players player :resources]
           resources (get-in game rpath)]
       (assoc-in game rpath
                 (merge-with op
                             resources
                             (select-keys r-map (keys resources)))))))

(defn dec-resources
  ([player r-map]
     (munge-resources - player r-map))
  ([game player-name r-map]
     (munge-resources - game player-name r-map)))

(defn inc-resources
  ([player r-map]
     (munge-resources + player r-map))
  ([game player r-map]
     (munge-resources + game player r-map)))


;; move counts

(defn move-count [game player]
  (get (frequencies (map :performed (vals (:slots game)))) player 0))

(defn rotate-while 
  "https://groups.google.com/forum/#!topic/clojure/SjmevTjZPcQ
   Rotates a collection left while (pred item) is true. Will return a unrotated 
   sequence if (pred item) is never true. Executes in O(n) time." 
  [pred coll] 
  (let [head (drop-while pred coll)] 
    (take (count coll) (concat head coll)))) 

(defn- rotate [n s] 
  (let [[front back] (split-at (mod n (count s)) s)] 
    (concat back front))) 


(defn get-starting-player [game]
  (first (filter :starting-player (vals (:players game)))))

(defn round-moves [game]
  (filter #(= (:round game) (:round %)) (:moves game)))

(defn remaining-moves [game]
  (let [r-moves (round-moves game)
        moves (frequencies (map :player r-moves))
        family-sizes (into {} (map (juxt :id :family) (vals (:players game))))]
    (merge-with - family-sizes moves)))

(defn round-done? [game]
  (let [remaining (remaining-moves game)]
    (not (some pos? (vals remaining)))))



(defn next-player-harvest [game]
  (let [harvest-moves (filter #(= :harvest (:type %)) (round-moves game))
        starting-player (get-starting-player game)
        players (rotate-while (comp not (:id starting-player)) (:player-order game))
        played-harvest (set (map :player harvest-moves))
        with-moves (filter (complement played-harvest) players)]
    (if (empty? with-moves)
      (throw (Exception. "Nothing to do! Must tick!"))
      (first with-moves))))

(defn next-player-game [game]
  (let [r-moves (round-moves game)
        starting-player (get-starting-player game)
        players (rotate-while (comp not (:id starting-player)) (:player-order game))
        remaining (remaining-moves game)]
    (if (empty? r-moves)
      (first players)
      (let [last-player (:player (last (:moves game)))
            rotated (rotate 1 (rotate-while (partial not= last-player) players))
            with-moves (filter #(pos? (remaining %)) rotated)]
        (first with-moves)))))

(defn next-move [game]
  (let [done (round-done? game)
        harvest-round ((set (:round-lengths game)) (:round game))]
    (cond
     (and done harvest-round) {:type :harvest :player (next-player-harvest game)}
     (not done) {:type :game :player (next-player-game game)}
     :else (throw (Exception. "Nothing to do! Must tick!")))))


;; space validators

(defn hut? [space]
  (:hut space))

(defn stable? [space]
  (:stable space))

(defn field? [space]
  (:field space))

(defn fences? [space]
  (some identity (filter identity (vals (:fences space)))))

(defn empty-space? [space]
  (not 
   (or
    (fences? space)
    (hut? space)
    (stable? space)
    (field? space))))

(defn last-move [game]
  (last (:moves game)))



(defn g-s [game move] ((:slots game) (:slot move)))
(defn g-p [game move] ((:players game) (:player move)))

(defn exposed-slot [game slot]
  "Has this slot been made available according to the number of rounds played"
  (boolean (some #{slot} (take (+ (:num-base-actions game) (:round game)) (:slot-order game)))))

(defn harvest-time [game]
  (#{(:round-lengths game)} (:round game)))


