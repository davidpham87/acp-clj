(ns acp-clj.acp-test
  (:require [clojure.test :refer [deftest is]]
            [acp-clj.acp :as acp]))

(deftest make-initialize-request-test
  (let [request (acp/make-initialize-request)]
    (is (= "initialize" (:method request)))
    (is (= "1.0" (get-in request [:params :protocolVersion])))
    (is (= true (get-in request [:params :clientCapabilities :fs :readTextFile])))
    (is (= true (get-in request [:params :clientCapabilities :fs :writeTextFile])))))

(deftest handle-response-test
  (let [client (acp/make-client :command "echo")
        response-received (promise)
        request-id 1]
    (swap! client assoc-in [:pending-requests request-id]
           {:on-success (fn [result] (deliver response-received result))})
    (#'acp/route-incoming-message client {:object {:id request-id :result "test-result"}})
    (is (= "test-result" (deref response-received 1000 :timeout)))))
