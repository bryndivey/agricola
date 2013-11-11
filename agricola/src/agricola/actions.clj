(ns agricola.actions
  (require [agricola.create :as create]
           [clojure.tools.logging :as log]))


(defn- get-count [action game]
  (:count (first (filter #(= (:action %) action) (:slots game)))))

(defn- update-action [game action new]
  (assoc game :slots
         (for [a (:slots game)]
           (if (= a action)
             new
             a))))

(defn- update-player [game player new]
  (assoc game :players
         (for [p (:players game)]
           (if (= p player)
             new
             p))))

(defn- g-m [game] (:move game))
(defn- g-a [game] (-> game g-m :action))
(defn- g-p [game] (-> game g-m :player))



(defn possible-move? [game]
  (let [action (g-a game)]
    (and 
     (not (:performed action))
     (or (not (:possible-fn action))
         ((:possible-fn action) game)))))

(defn perform-move [game]
  (assert (:perform-fn (g-a game)) "No perform action!")
  (let [action (g-a game)
        player (g-p game)
        [n-a n-p] ((:perform-fn action) game action player)
        n-a (assoc n-a :performed true)]
    (-> game
        (update-action action n-a)
        (update-player player n-p))))

(defn- perform-action-tick [game action]
  (if-let [tick-fn (:tick-fn action)]
    (do
      (log/info "Performing tick of " action)
      (update-action game action (assoc (tick-fn game action) :performed false)))
    game))

(defn perform-tick [game]
  (reduce #(perform-action-tick %1 %2) game (:slots game)))

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
   :perform-fn (fn [game action player]
                 (let [p (update-in player [:resources resource] + (:supply action))
                       a (assoc action :supply 0)]
                   [a p]))
   })


(defn set-move
  ([game player action]
     (set-move game player action {}))
  ([game player action args]
     (assoc game :move (merge args {:player player
                                    :action action}))))

(defn test-basic []
  (let [wood-3 (a-resource-sink "Three Wood" :wood 3)

        game (update-in (create/create-game) [:slots] conj wood-3)
        p1 (first (:players game))
        action (first (:slots game))]
    (let [n-g (perform-move (set-move game p1 action))]
      (assert (= 0 (-> n-g :slots first :supply)))
      (assert (= 3 (-> n-g :players first :resources :wood)))
      n-g)))


(defn print-actions [game])
