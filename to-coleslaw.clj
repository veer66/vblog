(require '[clojure.string :as str]
         '[rbre.core :as rbre]
         '[clostache.parser :refer [render]])

(defn list-dir [path]
  (->> (clojure.java.io/file path)
       file-seq
       (filter #(.isFile %) )
       (filter #(re-matches #".+\.md$" (.getName %)))))

(defn list-dir-by-year [prefix year]
  (map #(array-map :year year
                   :file %)
       (list-dir (str prefix "/" year))))

(defn read-post [file-year]
  (let [file (:file file-year)
        name (.getName file)
        content (->> file
                      clojure.java.io/reader
                      slurp)]
    (-> file-year
        (assoc :name name)
        (assoc :content content))))

(defn clean-title [title]
  (->> (str/replace title #"\s*#\s*" "")
       .trim))

(defn add-title [post]
  (->> (:content post)
       (re-seq #"#.+")
       first
       clean-title
       (assoc post :title)))

(defn clean-date [date]
  (->> (str/replace date #"[\t ]*Date:" "")
       .trim))

(defn add-date [post]
  (->> post
       :name
       (re-seq #"\d{4}-\d{2}-\d{1,2}")
       first
       (assoc post :date)))

(defn add-header [post]
  (assoc post
         :header        
         (render ";;;;;
title: {{title}}
tags: 
date: {{date}}
format: md
excerpt: 
;;;;;" {:title (:title post)
        :date (:date post)})))


(defn strip-content [content]
  (-> content
      (str/replace-first #"#.+" "")
      (str/replace-first #"Date:.+" "")))

;(str/replace "xxx\n # XX\n dddd" #"#.+" "--")

(defn strip-content-in-post [post]
  (update post
         :content
         strip-content))

(defn write-post [post]
  (spit (str (:year post)
             "/"
             (str/replace (:name post) #"\.md$" ".post")) 
        (str (:header post)
             "\n\n"
             (:content post))))

(transduce (comp (map read-post)
                 (map add-title)
                 (map add-date)
                 (map add-header)
                 (map strip-content-in-post))
           (fn [& args]
             (when (= 2 (count args))
               (write-post (second args))))
           (mapcat #(list-dir-by-year "../v21k-bak" %)
                   (map str (range 2014 2018))))


