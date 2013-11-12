(ns agricola.actions
  (require [agricola.game-old-actions :as agricola.game]
           [clojure.tools.logging :as log])
  (use [clojure.test]))


(def slots {:build-rooms 0
            :wood-3 1
            :plow 2})


(defn- get-count [action game]
  (:count (first (filter #(= (:action %) action) (:slots game)))))



(defn- update-player [game player new]
  (assoc game :players
         (for [p (:players game)]
           (if (= p player)
             new
             p))))

(defn- g-a [game move] (nth (:slots game) (:slot move)))
(defn- g-p [game move] ((:players game) (:player move)))
(defn- get-slot-num [move]
  (if (keyword? (:slot move))
    (assoc move :slot (slots (:slot move)))
    move))

(defn valid-move? [game move]
  ; should fail-out on first error but i don't know the codez for that
  (let [move (get-slot-num move)
        action (g-a game move)
        player (g-p game move)
        do-validate (fn [vfn]
                      (try
                        (vfn game action player (dissoc move :player :slot))
                        (catch Exception e false)))]
    (and 
     (not (:performed action))
     (or (not (:validate-fns action))
         (every? true? (map do-validate (:validate-fns action)))))))



(defn perform-move [game move]
  (let [move (get-slot-num move)
        action (g-a game move)
        player (g-p game move)]

    (assert (:perform-fn (g-a game move)) "No perform action!")
    (assert (valid-move? game move))
    
    (let [[n-a n-p] ((:perform-fn action) game action player (dissoc move :player :slot))
          n-a (assoc n-a :performed true)]
      (-> game
          (assoc-in [:slots (:slot move)] n-a)
          (assoc-in [:players (:player move)] n-p)))))

(defn- perform-action-tick [game a-num]
  (let [action (nth (:slots game) a-num)]
    (if-let [tick-fn (:tick-fn action)]
      (do
        (log/info "Performing tick of " action)
        (assoc-in game [:slots a-num] (assoc (tick-fn game action) :performed false)))
      game)))

(defn perform-tick [game]
  (reduce #(perform-action-tick %1 %2) game (range (count (:slots game)))))



(defn a-or-action [name a1 a2]
  {:name name
   :performed false
   :type "and-action"

   :actions [a1 a2]

   :perform-fn (fn [game action player args]
)
})

(defn a-resource-provider [name resource number]
  {:name name
   :performed false
   :type "resource-provider"
   
   :resource resource 
   :number number
   :perform-fn (fn [game action player args]
                 [action (update-in player [:resources resource] + number)])})

(defn a-resource-sink [name resource number]
  {:name name
   :performed false
   :type "resource-sink"
   
   :resource resource 
   :number number
   :supply number
   :tick-fn (fn [game action]
              (update-in action [:supply] + (:number action)))
   :perform-fn (fn [game action player args]
                 (let [p (update-in player [:resources resource] + (:supply action))
                       a (assoc action :supply 0)]
                   [a p]))
   })



(defn v-num-targets [min max]
  (fn [_ _ player args]
    (and (vector? (:targets args))
         (>= (count (:targets args)) min)
         (<= (count (:targets args)) max))))

(defn v-empty-targets [_ _ player args]
  (every? true? (map #(agricola.game/empty-space? (get-in player [:board %])) (:targets args))))

(defn a-plow [name]
  {:name name
   :performed false
   :type "plow"

   :validate-fns [(v-num-targets 1 1)
                  v-empty-targets]
   
   :perform-fn (fn [game action player args]
                 [action (assoc-in player [:board (first (:targets args)) :field] true)])})


(defn a-build-rooms [name]
  {:name name
   :performed false
   :type "build-rooms"

   :resource-counter (fn [game action player args]
                       (let [n (count (:targets args))
                             {:keys [type]} (agricola.game/count-huts player)]
                         {type (* 5 n)
                          :reed (* 2 n)}))

   :validate-fns [(v-num-targets 1 15)
                  v-empty-targets
                  (fn [game action player args]
                    (let [n (count (:targets args))
                          {:keys [count type]} (agricola.game/count-huts player)
                          req-resources ((:resource-counter action) game action player args)]
                      (and
                       ; no more than 5 huts
                       (<= (+ n count) 5)
                       ; sufficient resources
                       (agricola.game/has-at-least? player req-resources))))]

   :perform-fn (fn [game action player args]
                 (let [{:keys [type]} (agricola.game/count-huts player)
                       player (reduce #(agricola.game/add-hut %1 %2 type) player (:targets args))
                       req-resources ((:resource-counter action) game action player args)                       
                       player (agricola.game/dec-resources player req-resources)]
                   [action player]))})




(defn setup-game-for-test []
  (let [build-rooms (a-build-rooms "Build Rooms")
        wood-3 (a-resource-sink "Three Wood" :wood 3)
        plow (a-plow "Plow")]
    (assoc (agricola.game/create-game) :slots [build-rooms
                                               wood-3
                                               plow])))

(deftest t-resource-sink
  (let [m {:player :bryn :slot :wood-3}
        g1 (setup-game-for-test)
        g2 (perform-tick g1)
        g3 (perform-move g2 m)]
    
    (is (= true (valid-move? g1 m)))
    (is (= true (valid-move? g2 m)))
    (is (= false (valid-move? g3 m)))

    (is (= 3 (get-in g1 [:slots (slots :wood-3) :supply])))
    (is (= 6 (get-in g2 [:slots (slots :wood-3) :supply])))
    (is (= 0 (get-in g3 [:slots (slots :wood-3) :supply])))

    (is (= false (get-in g1 [:slots (slots :wood-3) :performed])))
    (is (= false (get-in g2 [:slots (slots :wood-3) :performed])))
    (is (= true (get-in g3 [:slots (slots :wood-3) :performed])))
    
    (is (= 0 (-> g1 :players :bryn :resources :wood)))
    (is (= 0 (-> g2 :players :bryn :resources :wood)))
    (is (= 6 (-> g3 :players :bryn :resources :wood)))))


(deftest t-plow
  (let [m {:player :bryn :slot :plow :targets [2]}
        g1 (setup-game-for-test)
        g2 (perform-move g1 m)]

    (is (= false (valid-move? g1 (assoc m :targets 0))))
    (is (= false (valid-move? g1 (assoc m :targets [0]))))
    (is (= false (valid-move? g1 (assoc m :targets [2 3]))))
    (is (= true (valid-move? g1 m)))

    (is (= true (get-in g2 [:players :bryn :board 2 :field])))))

(deftest t-build-rooms
  (let [m {:player :bryn :slot :build-rooms :targets [2]}
        g1 (setup-game-for-test)
        g2 (agricola.game/inc-resources g1 :bryn {:wood 5 :reed 2})
        g3 (perform-move g2 m)]

    (is (= false (valid-move? g2 (assoc m :targets 0))))
    (is (= false (valid-move? g2 (assoc m :targets [0]))))
    (is (= false (valid-move? g2 (assoc m :targets [2 3]))))
    (is (= true (valid-move? g2 m)))

    (is (= :wood (get-in g3 [:players :bryn :board 2 :hut])))
    (is (= 0 (get-in g3 [:players :bryn :resources :wood])))
    (is (= 0 (get-in g3 [:players :bryn :resources :reed])))
    ))




(defn test-basic []
  (let [
        wood-3 (a-resource-sink "Three Wood" :wood 3)
        plow (a-plow "Plow")
        game (update-in (agricola.game/create-game) [:slots] conj wood-3)
        game (update-in game [:slots] conj plow)]
    
    (let [m1 {:player :bryn :slot 1}
          m2 {:player :bryn :slot 2 :targets [2]}
          v1 (valid-move? game m1)
          v2 (valid-move? game m2)
          v (valid-move? game m2)
          g (perform-move game {:player :bryn :slot 1})
          g1 (perform-move g {:player :bryn :slot 2 :targets [2]})]
      (assert (= true v1))
      (assert (= true v2))
      (assert (= 0 (-> g1 :slots first :supply)))
      (assert (= 3 (-> g1 :players :bryn :resources :wood)))
      (assert (= true (-> g1 :slots second :performed)))
      (assert (= true (-> g1 :players :bryn :board (nth 2) :field)))
      g1)))


(deftest t-all
  (t-plow)
  (t-resource-sink))
