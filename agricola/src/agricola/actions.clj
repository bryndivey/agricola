(ns agricola.actions
  (require [agricola.game]
           [clojure.tools.logging :as log])
  (use [clojure.test]))


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


(defn valid-move? [game move]
  ; should fail-out on first error but i don't know the codez for that
  (let [action (g-a game move)
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
  (assert (:perform-fn (g-a game move)) "No perform action!")
  (assert (valid-move? game move))
  (let [action (g-a game move)
        player (g-p game move)
        [n-a n-p] ((:perform-fn action) game action player (dissoc move :player :slot))
        n-a (assoc n-a :performed true)]
    (-> game
        (assoc-in [:slots (:slot move)] n-a)
        (assoc-in [:players (:player move)] n-p))))

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
                 ;; todo VALIDATE
                 [action (assoc-in player [:board (first (:targets args)) :field] true)])})


(defn a-build-rooms [name]
  {:name name
   :performed false
   :type "build-rooms"

   :validate-fns [(v-num-targets 1 15)
                  v-empty-targets]
   })





(defn setup-game-for-test []
  (let [wood-3 (a-resource-sink "Three Wood" :wood 3)
        plow (a-plow "Plow")
        game (update-in (agricola.game/create-game) [:slots] conj wood-3)
        game (update-in game [:slots] conj plow)]
    game))


(deftest t-resource-sink
  (let [m {:player :bryn :slot 0}
        g1 (setup-game-for-test)
        g2 (perform-tick g1)
        g3 (perform-move g2 m)]
    
    (is (= true (valid-move? g1 m)))
    (is (= true (valid-move? g2 m)))
    (is (= false (valid-move? g3 m)))

    (is (= 3 (-> g1 :slots first :supply)))
    (is (= 6 (-> g2 :slots first :supply)))
    (is (= 0 (-> g3 :slots first :supply)))

    (is (= false (-> g1 :slots first :performed)))
    (is (= false (-> g2 :slots first :performed)))
    (is (= true (-> g3 :slots first :performed)))
    
    (is (= 0 (-> g1 :players :bryn :resources :wood)))
    (is (= 0 (-> g2 :players :bryn :resources :wood)))
    (is (= 6 (-> g3 :players :bryn :resources :wood)))))


(deftest t-plow
  (let [m {:player :bryn :slot 1 :targets [2]}
        g1 (setup-game-for-test)
        g2 (perform-move g1 m)]

    (is (= false (valid-move? g1 (assoc m :targets 0))))
    (is (= false (valid-move? g1 (assoc m :targets [0]))))
    (is (= false (valid-move? g1 (assoc m :targets [2 3]))))
    (is (= true (valid-move? g1 m)))

    (is (= true (get-in g2 [:players :bryn :board 2 :field])))))


(defn test-basic []
  (let [wood-3 (a-resource-sink "Three Wood" :wood 3)
        plow (a-plow "Plow")
        game (update-in (agricola.game/create-game) [:slots] conj wood-3)
        game (update-in game [:slots] conj plow)]
    
    (let [m1 {:player :bryn :slot 0}
          m2 {:player :bryn :slot 1 :targets [2]}
          v1 (valid-move? game m1)
          v2 (valid-move? game m2)
          v (valid-move? game m2)
          g (perform-move game {:player :bryn :slot 0})
          g1 (perform-move g {:player :bryn :slot 1 :targets [2]})]
      (assert (= true v1))
      (assert (= true v2))
      (assert (= 0 (-> g1 :slots first :supply)))
      (assert (= 3 (-> g1 :players :bryn :resources :wood)))
      (assert (= true (-> g1 :slots second :performed)))
      (assert (= true (-> g1 :players :bryn :board (nth 2) :field)))
      g1)))

