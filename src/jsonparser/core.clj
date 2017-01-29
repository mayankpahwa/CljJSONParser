(ns jsonparser.core
  (:require [clojure.string :as str])
  (:gen-class))

(declare object_parser)

(defn string_parser [data]
   (let [y (re-find #"^\".+\"" data)]
      (if y (str/trim (subs data (count y))))))

(defn num_parser [data]
   (let [y (or (re-find #"^\d+\.\d+" data) (re-find #"^-\d+\.\d+" data) (re-find #"^\d+" data) (re-find #"^-\d+" data))]
      (if y (str/trim (subs data (count y))))))

(defn null_parser [data]
   (if (str/starts-with? data "null") (str/trim (subs data 4))))

(defn colon_parser [data]
   (if (str/starts-with? data ":") (str/trim (subs data 1))))

(defn comma_parser [data]
   (if (str/starts-with? data ",") (str/trim (subs data 1))))

(defn boolean_parser [data]
   (cond 
      (str/starts-with? data "true") (str/trim (subs data 4))
      (str/starts-with? data "false") (str/trim (subs data 5))))

(defn array_parser [data]
   (if (str/starts-with? data "[")
      (loop [x (str/trim (subs data 1))]
         (if (str/starts-with? x "]")
            (str/trim (subs x 1))
            (let [y (or (string_parser x) (num_parser x) (object_parser x) (array_parser x) (boolean_parser x) (null_parser x))]
               (if y 
                  (let [z (comma_parser y)]
                     (cond 
                        z (if (not (str/starts-with? z "]")) (recur z))
                        :else (if (str/starts-with? y "]") (str/trim (subs y 1)))))))))))

(defn object_parser [data]
   (if (str/starts-with? data "{")
      (loop [x (str/trim (subs data 1))]
         (if (str/starts-with? x "}")
            (str/trim (subs x 1))
            (let [y (string_parser x)]
               (if y 
                  (let [z (colon_parser y)]
                     (if z
                        (let [a (or (string_parser z) (num_parser z) (object_parser z) (array_parser z) (boolean_parser z) (null_parser z))]
                           (if a
                              (let [b (comma_parser a)]
                                 (cond 
                                    b (if (not (str/starts-with? b "}")) (recur b))
                                    :else (if (str/starts-with? a "}") (str/trim (subs a 1)))))))))))))))

(defn -main
  []
  (let [data "2.4"]
      (let [tdata (str/trim data)]
         (let [output (or (string_parser tdata) (num_parser tdata) (object_parser tdata) (array_parser tdata) (boolean_parser tdata) (null_parser tdata))]
            (cond 
               (not output) (println "INVALID JSON") 
               :else (cond 
                     (= (count output) 0) (println "VALID JSON")
                     :else (println "INVALID JSON")))))))
