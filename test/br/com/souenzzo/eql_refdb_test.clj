(ns br.com.souenzzo.eql-refdb-test
  (:require [br.com.souenzzo.eql-refdb :as refdb]
            [clojure.test :refer [deftest is testing]]
            [clojure.pprint :as pp]))

(deftest db->tree
  (is (= {:user/contatcs [{:user/name "refdb"}]
          :user/id       1
          :user/name     "refdb"}
         (refdb/db->tree
           {::refdb/db    {:user/id {1 {:user/id       1
                                        :user/name     "refdb"
                                        :user/contatcs [[:user/id 1]]}}}
            ::refdb/query [{[:user/id 1] [:user/id
                                          :user/name
                                          {:user/contatcs [:user/name]}]}]})))
  (is (= {:user/contatcs {:user/name "refdb"}
          :user/id       1
          :user/name     "refdb"}
         (refdb/db->tree
           {::refdb/db    {:user/id {1 {:user/id       1
                                        :user/name     "refdb"
                                        :user/contatcs [:user/id 1]}}}
            ::refdb/query [{[:user/id 1] [:user/id
                                          :user/name
                                          {:user/contatcs [:user/name]}]}]})))
  (is (= {:user/contatcs {:user/contatcs {:user/contatcs {:user/contatcs {:user/name "refdb"}}}
                          :user/name     "refdb"}
          :user/id       1
          :user/name     "refdb"}
         (refdb/db->tree
           {::refdb/db    {:user/id {1 {:user/id       1
                                        :user/name     "refdb"
                                        :user/contatcs [:user/id 1]}}}
            ::refdb/query [{[:user/id 1] [:user/id
                                          :user/name
                                          {:user/contatcs [:user/name
                                                           {:user/contatcs [{:user/contatcs [{:user/contatcs [:user/name]}]}]}]}]}]}))))


(deftest tree->db
  (is (= {:user/id {1 {:user/id   1
                       :user/name "refdb"}}}
         (refdb/tree->db
           {::refdb/attribute->index {}
            ::refdb/value            {[:user/id 1] {:user/id   1
                                                    :user/name "refdb"}}
            ::refdb/query            [{[:user/id 1] [:user/id
                                                     :user/name]}]})))
  (is (= {:user/id {1 {:user/id       1
                       :user/name     "refdb"
                       :user/contatcs [[:user/id 1]]}}}
         (refdb/tree->db
           {::refdb/attribute->index {:user/contatcs :user/id}
            ::refdb/value            {[:user/id 1] {:user/id       1
                                                    :user/name     "refdb"
                                                    :user/contatcs [{:user/id 1}]}}
            ::refdb/query            [{[:user/id 1] [:user/id
                                                     :user/name
                                                     {:user/contatcs [:user/id]}]}]})))
  (is (= {:user/id {1 {:user/id       1
                       :user/name     "refdb"
                       :user/contatcs [[:user/id 1]
                                       [:user/id 2]]}
                    2 {:user/id   2
                       :user/name "refrefdb"}}}
         (-> (refdb/tree->db
               {::refdb/attribute->index {:user/contatcs :user/id}
                ::refdb/value            {[:user/id 1] {:user/id       1
                                                        :user/name     "refdb"
                                                        :user/contatcs [{:user/id   1
                                                                         :user/name "refdb"}
                                                                        {:user/id   2
                                                                         :user/name "refrefdb"}]}}
                ::refdb/query            [{[:user/id 1] [:user/id
                                                         :user/name
                                                         {:user/contatcs [:user/id
                                                                          :user/name]}]}]})))))