(ns agricola-client.improvements-test
  (:require [clojure.test :refer :all]
            [agricola-client.improvements :as improvements]
            [agricola-client.actions :as actions]))

(deftest t-bake-action
  (let [pfn (actions/get-action-fn :bake :perform)
        vfns (actions/get-action-fn :bake :validate)
        imp {:min 1 :max 2 :bakes 3}

        p (pfn nil {:resources {:grain 1 :food 0}} imp {:targets [{:resource :grain}]})]
    
    (is (= true (actions/validators-pass? vfns nil nil imp
                                          {:targets [{:resource :grain}]})))
    (is (= false (actions/validators-pass? vfns nil nil imp {:targets []})))
    (is (= false (actions/validators-pass? vfns nil nil imp
                                           {:targets [{:resource :grain}
                                                      {:resource :grain}
                                                      {:resource :grain}]})))

    (is (= (-> p :player :resources) {:grain 0 :food 3}))
    ))
