(ns hyperfiddle.domain
  (:require
    [bidi.bidi :as bidi]
    [clojure.set :as set]
    [hyperfiddle.database.color :as color]))


(defprotocol Domain
  (basis [domain])
  (type-name [domain])
  (fiddle-dbname [domain])
  (databases [domain])
  (environment [domain])
  (url-decode [domain s])
  (url-encode [domain route])
  (api-routes [domain])

  (system-fiddle? [domain fiddle-ident])
  (hydrate-system-fiddle [domain fiddle-ident])
  )

(defn database [domain dbname]
  (get (databases domain) dbname))

(defn database-color [domain dbname]
  (or (:database/color (database domain dbname)) (color/color-for-name dbname)))

(defn api-path-for [domain handler & {:as params}]
  (apply bidi/path-for (api-routes domain) handler (apply concat params)))

(defn api-match-path [domain path & {:as options}]
  (apply bidi/match-route (api-routes domain) path (apply concat options)))

(defn valid-dbname? [domain dbname] (some? (database domain dbname)))

(defn valid-dbnames? [domain dbnames] (set/subset? (set dbnames) (set (keys (databases domain)))))

(defn dbname-label [dbname]
  (if (= "$" dbname)
    "$ (default)"
    dbname))
