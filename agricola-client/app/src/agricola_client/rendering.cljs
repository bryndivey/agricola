(ns agricola-client.rendering
  (:require [domina :as dom]
            [domina.css :refer [sel]]
            [io.pedestal.app.render.push :as render]
            [io.pedestal.app.render.push.templates :as templates]
            [io.pedestal.app.render.push.handlers.automatic :as d]
            [io.pedestal.app.util.log :as log])
  (:require-macros [agricola-client.html-templates :as html-templates]))

;; Load templates.

(def templates (html-templates/agricola-client-templates))


(defn render-board [renderer [_ path] transmitter]
  (let [parent (render/get-parent-id renderer path)
        id (render/new-id! renderer path)
        html (templates/add-template renderer path (:board-page templates))]
    (dom/append! (dom/by-id "content") (html {:id id :slots "Slots are here"}))))


;; slots

(defn render-slot [renderer [_ path] transmitter]
  (let [parent (render/get-parent-id renderer path)
        id (render/new-id! renderer path)
        html (templates/add-template renderer path (:slot templates))
        content (html {:id id :action "No action" :performed "Dunno"})]
    (dom/append! (dom/by-id "slots") content)))

(defn update-slot [renderer [_ path _ new-value] transmitter]
  (templates/update-t renderer path {:action (str (:action new-value))
                                     :performed (str (:performed new-value))}))



(defn render-player [r [_ p] t]
  (let [id (render/new-id! r p)
        html (templates/add-template r p (:player templates))]
    (dom/append! (dom/by-id "players") (html {:id id :name "None yet"}))
    (dom/destroy-children! (second (dom/children (dom/by-id id))))))

(defn update-player [renderer [_ path _ new-value] transmitter]
  (let [key (last path)
        path (vec (butlast path))]
    (log/error :path player :key key :new-value (str new-value))
    (templates/update-t renderer path {key (str new-value)})))

(defn render-player-space [r [_ p] t]
  (let [parent (render/get-id r (vec (butlast (butlast p))))
        board (second (dom/children (dom/by-id parent)))
        id (render/new-id! r p)
        html (templates/add-template r p (:board-space templates))]
    (log/error "RENDER" parent board)
    (dom/append! board (html {:space "None"}))))

(defn update-player-space [renderer [_ path _ new-value] transmitter]
  (let [hut (and (:hut new-value) (str (:hut new-value) " hut"))
        stable (and (:stable new-value) "stable")
        field (and (:field new-value) "field")]
    (log/error "UPDATE" path :val new-value)
    (templates/update-t renderer path {:space (str hut stable field)})))

;; The data structure below is used to map rendering data to functions
;; which handle rendering for that specific change. This function is
;; referenced in config/config.edn and must be a function in order to
;; be used from the tool's "render" view.

(defn render-config []
  [[:node-create  [:main :game] render-board]
   [:node-destroy   [:main :game] d/default-exit]
   
   [:node-create [:main :game :slots :*] render-slot]
   [:node-destroy [:main :game :slots :*] d/default-exit]
   [:value [:main :game :slots :*] update-slot]

   [:node-create [:main :game :players :* :board :*] render-player-space]
   [:value [:main :game :players :* :board :*] update-player-space]
   
   [:node-create [:main :game :players :*] render-player]
   [:value [:main :game :players :**] update-player]
   
])

;; In render-config, paths can use wildcard keywords :* and :**. :*
;; means exactly one segment with any value. :** means 0 or more
;; elements.
