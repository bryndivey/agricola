{:config {:order 1, :description ":slots", :name :slots}
 :data
 [
  [:node-create [] :map]
  [:node-create [:main] :map]
  [:node-create [:main :game] :map]
  [:node-create [:main :game :game-id] :map]
  [:value [:main :game :game-id] nil 1]
  [:node-create [:main :game :slots] :map]
  [:node-create [:main :game :slots :starting-player] :map]
  [:value [:main :game :slots :starting-player] nil {:action :starting-player, :performed false}]
  [:node-create [:main :game :slots :build-stable] :map]
  [:value [:main :game :slots :build-stable] nil {:action :build-stable, :performed false}]
  [:node-create [:main :game :slots :one-grain] :map]
  [:value [:main :game :slots :one-grain] nil {:action :one-grain, :performed false}]
  [:node-create [:main :game :slots :three-wood] :map]
  [:value [:main :game :slots :three-wood] nil {:action :three-wood, :performed false, :supply 3}]
  [:node-create [:main :game :slots :build-rooms] :map]
  [:value [:main :game :slots :build-rooms] nil {:action :build-rooms, :performed false}]
  [:node-create [:main :game :slots :plow] :map]
  [:value [:main :game :slots :plow] nil {:action :plow, :performed false}]
  :break
  [:value [:main :tick] nil 2]
  :break
  [:value [:main :game :slots :three-wood] {:action :three-wood, :performed false, :supply 3} {:action :three-wood, :performed false, :supply 6}]
  :break
  [:value [:main :tick] 2 3]
  :break
  [:value [:main :game :slots :three-wood] {:action :three-wood, :performed false, :supply 6} {:action :three-wood, :performed false, :supply 9}]
  :break
  [:value [:main :game :slots :one-grain] {:action :one-grain, :performed false} {:action :one-grain, :performed :bryn}]
 ]
 :bak
 [
  ]

 }
