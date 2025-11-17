(ns acp-clj.runner
  (:require
   [clojure.test :as t]))

(def test-namespaces '[acp-clj.core-test])

(defn -main [& _]
  (doseq [test-ns test-namespaces] (require test-ns))
  (let [{:keys [fail error]}
        (apply t/run-tests test-namespaces)]
    (when (and fail error (pos? (+ fail error)))
      (System/exit 1))))
