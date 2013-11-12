(ns agricola-client.simulated.services
  (:require [io.pedestal.app.protocols :as p]
            [io.pedestal.app.messages :as msg]
            [io.pedestal.app.util.log :as log]
            [agricola-client.actions :as actions]))

(defn perform-move [input-queue game move]
  (let [new-game (actions/perform-move game move)]
    (p/put-message input-queue {msg/type :swap
                                msg/topic [:game]
                                :value new-game})))

(defn services-fn [message input-queue]
  (log/info "Message: " (msg/type message))
  (log/info "Move: " (:move message))
  (cond
   (= (msg/type message) :perform-move) (perform-move input-queue
                                                      (:game message)
                                                      (:move message))
   :else (log/info "Received unprocessed message: " message))
  )
