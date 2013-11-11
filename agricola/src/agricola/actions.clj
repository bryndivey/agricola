(ns agricola.actions
  (require [agricola.create :as create]
           [clojure.tools.logging :as log]))


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


(defn possible-move? [game move]
  (let [action (g-a game move)
        ]
    (and 
     (not (:performed action))
     (or (not (:possible-fn action))
         ((:possible-fn action) game move)))))

(defn perform-move [game move]
  (assert (:perform-fn (g-a game move)) "No perform action!")
  
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

(defn a-resource-sink [name resource number]
  {:name name
   :performed false
   :type "resource-sink"
   
   :resource resource 
   :number number
   :supply number
   :tick-fn (fn [game action]
              (update-in action [:supply] + (:number action)))
   :perform-fn (fn [game action player]
                 (let [p (update-in player [:resources resource] + (:supply action))
                       a (assoc action :supply 0)]
                   [a p]))
   })

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

(defn a-plow [name]
  {:name name
   :performed false
   :type "plow"
   
   :perform-fn (fn [game action player args]
                 ;; todo VALIDATE
                 [action (assoc-in player [:board (:target args) :field] true)])})


(defn test-basic []
  (let [wood-3 (a-resource-sink "Three Wood" :wood 3)
        plow (a-plow "Plow")
        game (update-in (create/create-game) [:slots] conj wood-3)
        game (update-in game [:slots] conj plow)]
    
    (let [g (perform-move game {:player :bryn :slot 0})
          g1 (perform-move g {:player :bryn :slot 1 :target 2})]
      (assert (= 0 (-> g1 :slots first :supply)))
      (assert (= 3 (-> g1 :players :bryn :resources :wood)))
      (assert (= true (-> g1 :slots second :performed)))
      (assert (= true (-> g1 :players :bryn :board (nth 2) :field)))
      g1)))


(defn print-actions [game])
