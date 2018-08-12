(ns chia.static.assets
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.security MessageDigest)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Asset handling

;; Dynamic vars are used for asset-path options


(def ^:dynamic *content-hashes?*
  "When true, append content-hashes to asset paths."
  false)

(def ^:dynamic *output-dir*
  "Local directory where assets are written"
  nil)

(def ^:dynamic *asset-path*
  "Path where assets are to be accessed by client"
  nil)

(defn public-path []
  (if *asset-path*
    (str/replace *output-dir* (re-pattern (str *asset-path* "$")) "")
    *output-dir*))

(defn strip-slash [path]
  (cond-> path
          (str/starts-with? path "/") (subs 1)))

(defn md5
  "Returns md5 hash for string"
  ;; from https://gist.github.com/jizhang/4325757#gistcomment-2196746
  [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn try-slurp [file]
  (try (slurp file)
       (catch Exception e nil)))

(defn asset-file [path]
  (assert *output-dir* "*asset-dir* must be set")
  (io/file *output-dir* (strip-slash path)))

(defn read-asset
  "Returns the contents for an asset"
  [path]
  (-> (asset-file path)
      (try-slurp)))

(defn asset-path
  "Asset-path function, for use in generating HTML"
  [path]
  (if-not (str/starts-with? path "/")
    path
    (str
     path
     (when *content-hashes?*
       (some->> (io/file (public-path) (strip-slash path))
                (try-slurp)
                (md5)
                (str "?v="))))))

(defn write-asset!
  "Write `content` string to an asset file"
  [path content]
  (doto (asset-file path)
    (io/make-parents)
    (spit content))
  (println (str " + " path)))

(def html-page nil)