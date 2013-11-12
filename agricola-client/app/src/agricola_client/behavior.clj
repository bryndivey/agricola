(ns ^:shared agricola-client.behavior
    (:require [clojure.string :as string]
              [io.pedestal.app :as app]
              [io.pedestal.app.dataflow :as d]
              [io.pedestal.app.messages :as msg]
              [io.pedestal.app.util.log :as log]))

;; TRANSFORMS

(defn swap-transform [_ message]
  (:value message))

(defn move-transform [t message]
  (select-keys message [:player :slot :targets]))

(defn inc-transform [_ message]
  ((fnil inc 1) (:value message)))


;; INITIALIZERS

(defn init-game [_]
  [[:node-create [:move] :map]

   [:transform-enable [:resources]
    :swap [{msg/topic [:game :players :bryn :resources]
            :value {:food 100 :wood 100 :clay 100 :reed 100 :stone 100 :grain 100 :vegetable 100}}]]

   [:transform-enable [:tick]
    :request-tick [{msg/topic [:tick]}]]
   
   [:transform-enable [:move-1]
    :perform-move [{msg/topic [:requested-move]
                    (msg/param :player) {:read-as :data}
                    (msg/param :slot) {:read-as :data}
                    (msg/param :targets) {:read-as :data}}]]])

(defn send-move [inputs]
  (let [message (:message inputs)
        game (get-in inputs [:new-model :game])]
    [{msg/type :perform-move :msg/topic [:game] :game game :move (select-keys
                                                                  message
                                                                  [:slot :player :targets])}]))

(defn send-tick [inputs]
  [{msg/type :perform-tick :msg/topic [:game] :game (get-in inputs [:new-model :game])}])


(defn player-emit [inputs]
  (let [new (merge (d/added-inputs inputs) (d/updated-inputs inputs))]
    (log/debug new))

  [])

(def example-app
  {:version 2
   
   :transform [[:swap [:**] swap-transform]
               [:perform-move [:requested-move] move-transform]
               [:request-tick [:tick] inc-transform]]

   :effect #{{:in #{[:requested-move]}
              :fn send-move}
             {:in #{[:tick]} :fn send-tick}}
   
   :continue #{[#{}]}

   :emit [{:init init-game}
          [#{[:requested-move]
             [:tick]

             [:error]
             
             [:game :game-id]
             [:game :slots :*]

             [:game :players :* :name]
             [:game :players :* :resources]
             [:game :players :* :animals]
             [:game :players :* :starting-player]
             [:game :players :* :board :*]
             } (app/default-emitter [:main])]
          ]
   })
