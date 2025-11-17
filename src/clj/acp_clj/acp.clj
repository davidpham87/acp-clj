(ns acp-clj.acp
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:const acp-package-version "0.7.2")
(def ^:const jsonrpc-version "2.0")

(defonce acp-logging-enabled (atom true))
(defonce instance-count (atom 0))

(defn- increment-instance-count []
  (swap! instance-count inc))

(defn- parse-json [json-str]
  (try
    (json/parse-string json-str true)
    (catch Exception e
      (println (str "JSON PARSE ERROR: Invalid JSON: " json-str)))))

(defn- serialize-json [data]
  (str (json/generate-string data) "\n"))

(defn- route-incoming-message [client message]
  (let [object (:object message)
        id (:id object)
        result (:result object)
        error (:error object)
        method (:method object)]
    (cond
      (and id (contains? object :result))
      (let [pending-request (get (:pending-requests @client) id)]
        (when-let [on-success (:on-success pending-request)]
          (on-success result))
        (swap! client update :pending-requests dissoc id))

      (and id error)
      (let [pending-request (get (:pending-requests @client) id)]
        (when-let [on-failure (:on-failure pending-request)]
          (on-failure error))
        (swap! client update :pending-requests dissoc id))

      (and method id)
      (doseq [handler (:request-handlers @client)]
        (handler object))

      (and method (not id))
      (doseq [handler (:notification-handlers @client)]
        (handler object))

      :else
      (println "Unknown message type: " object))))

(defn make-client
  "Create an ACP client. Returns an atom containing the client state."
  [& {:keys [command command-params environment-variables]}]
  (when-not command
    (throw (Exception. ":command is required")))
  (atom
   {:instance-count (increment-instance-count)
    :process nil
    :command command
    :command-params command-params
    :environment-variables environment-variables
    :pending-requests {}
    :request-id (atom 0)
    :notification-handlers []
    :request-handlers []
    :error-handlers []}))

(defn- client-started? [client]
  (let [process (:process @client)]
    (and process (.isAlive process))))

(defn- start-client
  "Start the client process."
  [client]
  (let [command (:command @client)
        command-params (:command-params @client)
        environment-variables (:environment-variables @client)
        pb (ProcessBuilder. (into [command] command-params))
        env (.environment pb)]
    (when-not (.exists (io/file command))
      (throw (Exception. (str "\"" command "\" command line utility not found. Please install it"))))
    (doseq [env-var environment-variables]
      (let [[k v] (str/split env-var #"=" 2)]
        (.put env k v)))
    (let [process (.start pb)]
      (swap! client assoc :process process)
      (let [stdout-reader (io/reader (.getInputStream process))
            stderr-reader (io/reader (.getErrorStream process))]
        (future
          (with-open [rdr stdout-reader]
            (doseq [line (line-seq rdr)]
              (when-let [object (parse-json line)]
                (let [message {:json line :object object}]
                  (route-incoming-message client message))))))
        (future
          (with-open [rdr stderr-reader]
            (doseq [line (line-seq rdr)]
              (println (str "STDERR: " line)))))))))

(defn send-request
  "Send a request from the client."
  [& {:keys [client request on-success on-failure]}]
  (when-not (client-started? client)
    (start-client client))
  (let [request-id (swap! (:request-id @client) inc)
        full-request (assoc request
                              :jsonrpc jsonrpc-version
                              :id request-id)]
    (swap! client update :pending-requests assoc request-id {:request full-request
                                                             :on-success on-success
                                                             :on-failure on-failure})
    (let [json (serialize-json full-request)
          process (:process @client)
          writer (io/writer (.getOutputStream process))]
      (.write writer json)
      (.flush writer))))

(defn make-initialize-request
  "Instantiate an \"initialize\" request."
  [& {:keys [protocol-version read-text-file-capability write-text-file-capability]
      :or {protocol-version "1.0"
           read-text-file-capability true
           write-text-file-capability true}}]
  {:method "initialize"
   :params {:protocolVersion protocol-version
            :clientCapabilities {:fs {:readTextFile read-text-file-capability
                                      :writeTextFile write-text-file-capability}}}})
