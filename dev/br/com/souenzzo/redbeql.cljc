(ns br.com.souenzzo.redbeql
  (:require [edn-query-language.core :as eql]
            [br.com.souenzzo.eql-refdb :as refdb]
            [com.wsscode.pathom.core :as p]
            [re-frame.core :as rf]
            [clojure.core.async :as async]
            [com.wsscode.pathom.connect :as pc]))


(defn on-result
  [db [_ tx result]]
  (let [query (-> {:type     :root
                   :children (into []
                                   (mapcat (fn [{:keys [type children]
                                                 :as   node}]
                                             (if (= type :call)
                                               children
                                               [node])))
                                   (:children (eql/query->ast tx)))}
                  eql/ast->query)
        value (into {}
                    (mapcat (fn [[k v]]
                              (if (symbol? k)
                                v
                                {k v})))
                    result)]
    (refdb/tree->db
      {::refdb/db    db
       ::refdb/value value
       ::refdb/query query})))

(defn eql
  [{::keys [on-result] :as env}]
  (let [env (merge {::on-result              ::on-result
                    ::p/reader               [p/map-reader
                                              pc/reader2
                                              pc/open-ident-reader
                                              p/env-placeholder-reader]
                    ::p/placeholder-prefixes #{">"}}
                   env)
        parser (p/parallel-parser (merge
                                    {::p/plugins [(pc/connect-plugin env)]
                                     ::p/mutate  pc/mutate
                                     ::p/env     env}
                                    env))]
    (fn [tx]
      (let [result (async/<! (parser env tx))]
        (rf/dispatch [on-result tx result])))))
