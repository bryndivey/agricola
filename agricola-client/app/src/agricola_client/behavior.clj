(ns ^:shared agricola-client.behavior
    (:require [clojure.string :as string]
              [io.pedestal.app :as app]
              [io.pedestal.app.messages :as msg]
              [io.pedestal.app.util.log :as log]))

;; TRANSFORMS

(defn swap-transform [_ message]
  (:value message))

(defn move-transform [t message]
  (select-keys message [:player :slot]))

(defn inc-transform [_ message]
  ((fnil inc 1) (:value message)))


;; INITIALIZERS

(defn init-game [_]
  [[:node-create [:move] :map]
   [:transform-enable [:tick]
    :tick [{msg/topic [:request-tick]}]] 
   [:transform-enable [:move]
    :perform-move [{msg/topic [:requested-move]
                    (msg/param :player) {:read-as :data}
                    (msg/param :slot) {:read-as :data}}]]])

(defn send-move [inputs]
  (let [message (:message inputs)
        game (get-in inputs [:new-model :game])]
    [{msg/type :perform-move :msg/topic [:game] :game game :move (select-keys
                                                                  message
                                                                  [:slot :player])}]))

(defn send-tick [message]
  (if (:tick message)
    [{msg/type :perform-tick :msg/topic [:game]}]))

(def example-app
  {:version 2
   
   :transform [[:swap [:game] swap-transform]
               [:perform-move [:requested-move] move-transform]
               [:tick [:request-tick] inc-transform]]

   :effect #{{:in #{[:requested-move]}
              :fn send-move}
;             [{[:requested-move] :move [:game] :game} send-move :map]
             [{[:tick] :tick} send-tick :map]}
   
   :continue #{[#{}]}

   :emit [{:init init-game}
          [#{[:requested-move]
             [:game :game-id]
             [:game :players :*]
             [:game :slots]} (app/default-emitter [:main])]]

   })
