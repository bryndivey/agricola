(ns agricola-client.simulated.services
  (:require [io.pedestal.app.protocols :as p]
            [io.pedestal.app.messages :as msg]
            [io.pedestal.app.util.log :as log]
            [agricola-client.actions :as actions]))

(defn new-game [input-queue game]
  (p/put-message input-queue {msg/type :swap
                              msg/topic [:game]
                              :value game}))

(defn perform-move [input-queue game move]
  (comment p/put-message input-queue {msg/type :swap msg/topic [:requested-move] :value nil})
  (let [valid (actions/valid-move? game move)]
    (if valid
      (let [n (actions/perform-move game move)]
        (log/info "NEW GAME" n)
        (new-game input-queue n))
      (p/put-message input-queue {msg/type :swap
                                  msg/topic [:error]
                                  :value "INVALID MOVE!"}))))

(defn tick [input-queue game]
  (let [n (actions/game-tick game)]
    (new-game input-queue n)))

(defn services-fn [message input-queue]
  (log/info "RECEIVED MESSAGE " (msg/type message))
  (let [type (msg/type message)]
    (cond
     (= type :perform-move) (perform-move input-queue
                                          (:game message)
                                          (:move message))
     (= type :perform-tick) (tick input-queue (:game message))
     :else (log/error "Unhandled message of type" type))))
