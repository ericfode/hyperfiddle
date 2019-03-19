(ns hypercrud.transit
  (:require
    [cats.core :as cats]
    [cats.monad.either :as either #?@(:cljs [:refer [Left Right]])]
    [cats.monad.exception :as exception #?@(:cljs [:refer [Failure Success]])]
    [cognitect.transit :as t]
    #?(:cljs [com.cognitect.transit.types])
    [contrib.datomic :refer [->Schema #?(:cljs Schema)]]
    [contrib.uri :refer [->URI #?(:cljs URI)]]
    [hypercrud.types.DbName :refer [->DbName #?(:cljs DbName)]]
    [hypercrud.types.DbRef :refer [->DbRef #?(:cljs DbRef)]]
    [hypercrud.types.EntityRequest :refer [->EntityRequest #?(:cljs EntityRequest)]]
    [hypercrud.types.Err :refer [->Err #?(:cljs Err)]]
    [hypercrud.types.QueryRequest :refer [->QueryRequest #?(:cljs QueryRequest)]]
    [hypercrud.types.ThinEntity :refer [->ThinEntity #?(:cljs ThinEntity)]]
    [hyperfiddle.domains.bidi :refer [map->BidiDomain #?(:cljs BidiDomain)]]
    [hyperfiddle.domains.ednish :refer [map->EdnishDomain #?(:cljs EdnishDomain)]])
  #?(:clj
     (:import
       (cats.monad.either Left Right)
       (cats.monad.exception Failure Success)
       (clojure.lang ExceptionInfo)
       (contrib.datomic Schema)
       (hypercrud.types.DbName DbName)
       (hypercrud.types.DbRef DbRef)
       (hypercrud.types.EntityRequest EntityRequest)
       (hypercrud.types.Err Err)
       (hypercrud.types.QueryRequest QueryRequest)
       (hypercrud.types.ThinEntity ThinEntity)
       (hyperfiddle.domains.bidi BidiDomain)
       (hyperfiddle.domains.ednish EdnishDomain)
       (java.io ByteArrayInputStream ByteArrayOutputStream))))


(def read-handlers
  {
   "schema" (t/read-handler #(contrib.datomic/indexed-schema (vec %))) ; it's a javascript array wut
   "DbName" (t/read-handler ->DbName)
   "DbRef" (t/read-handler #(apply ->DbRef %))
   "EReq" (t/read-handler #(apply ->EntityRequest %))
   "err" (t/read-handler ->Err)
   "QReq" (t/read-handler #(apply ->QueryRequest %))
   "entity" (t/read-handler #(apply ->ThinEntity %))
   "r" (t/read-handler ->URI)
   "left" (t/read-handler #(either/left %))
   "right" (t/read-handler #(either/right %))
   "failure" (t/read-handler #(exception/failure %))
   "success" (t/read-handler #(exception/success %))
   "left-v" (t/read-handler #(apply either/left %))
   "right-v" (t/read-handler #(apply either/right %))
   "failure-v" (t/read-handler #(apply exception/failure %))
   "success-v" (t/read-handler #(apply exception/success %))
   "ex-info" (t/read-handler #(apply ex-info %))
   "BidiDomain" (t/read-handler map->BidiDomain)
   "EdnishDomain" (t/read-handler map->EdnishDomain)
   })

(def write-handlers
  {
   Schema (t/write-handler (constantly "schema") (fn [^Schema v] (.-schema-pulledtree v)))
   DbName (t/write-handler (constantly "DbName") (fn [v] (:dbname v)))
   DbRef (t/write-handler (constantly "DbRef") (fn [v] [(:dbname v) (:branch v)]))
   EntityRequest (t/write-handler (constantly "EReq") (fn [v] [(:e v) (:db v) (:pull-exp v)]))
   Err (t/write-handler (constantly "err") #(:msg %))
   QueryRequest (t/write-handler (constantly "QReq") (fn [v] [(:query v) (:params v)]))
   ThinEntity (t/write-handler (constantly "entity") (fn [^ThinEntity v] [(.-dbname v) (.-id v)]))
   Left (t/write-handler (constantly "left-v") (fn [v] (vector (cats/extract v))))
   Right (t/write-handler (constantly "right-v") (fn [v] (vector (cats/extract v))))
   Failure (t/write-handler (constantly "failure-v") (fn [v] (vector (cats/extract v))))
   Success (t/write-handler (constantly "success-v") (fn [v] (vector (cats/extract v))))
   ExceptionInfo (t/write-handler (constantly "ex-info") (fn [ex] [(ex-message ex) (ex-data ex) (ex-cause ex)]))
   BidiDomain (t/write-handler (constantly "BidiDomain") #(into {} %))
   EdnishDomain (t/write-handler (constantly "EdnishDomain") #(into {} %))
   #?@(:cljs [URI (t/write-handler (constantly "r") (fn [v] (.-uri-str v)))])
   })

(def ^:dynamic *string-encoding* "UTF-8")

(defn decode
  "Transit decode an object from `s`."
  [s & {:keys [type opts]
        :or {type :json opts {:handlers read-handlers}}}]
  #?(:clj  (let [in (ByteArrayInputStream. (.getBytes s *string-encoding*))
                 rdr (t/reader in type opts)]
             (t/read rdr))
     :cljs (let [rdr (t/reader type opts)]
             (t/read rdr s))))

(defn encode
  "Transit encode `x` into a String."
  [x & {:keys [type opts]
        :or {type :json opts {:handlers write-handlers}}}]
  #?(:clj  (let [out (ByteArrayOutputStream.)
                 writer (t/writer out type opts)]
             (t/write writer x)
             (.toString out))
     :cljs (let [wrtr (t/writer type opts)]
             (t/write wrtr x))))

#?(:cljs (extend-type com.cognitect.transit.types/UUID IUUID)) ; https://github.com/hyperfiddle/hyperfiddle/issues/728
