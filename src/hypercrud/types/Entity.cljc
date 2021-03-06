(ns hypercrud.types.Entity
  #?(:clj
     (:import [clojure.lang Counted IFn IHashEq ILookup Seqable]
              [clojure.core.protocols IKVReduce])))


(defn- impl-hash [o]
  ; Omit tx-tempid, that would be a big problem if it wasn't the same.
  (hash [(.-uri o) (:db/id (.-coll o))]))

(defn- impl-print [o]
  (pr-str (.-coll o)))

(declare equals-impl)

; Entity is always a ref
; EntityVal makes no sense. Even though this is backed by a PTM, it's semantically a ref into graph
; Purpose of dbval in scope is for URL generation, which needs tempid map, which was hydrated
; which is a DBRef, because the staging area is
; passed alongside
; Depart from Datomic here, because Hyperfiddle always has a staging area in context.

; NO! DbVals have tempid maps.


; Whole purpose of Entity is: a ref type, with a dbval, which has a tempid map, for rendering URLs.
; What if Entity had its own old tempid? And that's it?
; TODO access only through reactive cursors
(deftype Entity [uri coll]
  #?@(:clj  [Object
             (equals [o other] (equals-impl o other))

             IHashEq
             (hasheq [o] (impl-hash o))

             Seqable
             (seq [o] (seq coll))

             Counted
             (count [o] (count coll))

             ILookup
             (valAt [o k] (get coll k))
             (valAt [o k not-found] (get coll k not-found))

             IKVReduce
             (kv_reduce [o f init] (reduce-kv coll f init))

             IFn
             (invoke [o k] (coll k))
             (invoke [o k not-found] (coll k not-found))]

      :cljs [Object
             (toString [o] (impl-print o))

             IPrintWithWriter
             (-pr-writer [o writer _] (-write writer (.toString o)))

             IEquiv
             (-equiv [o other] (equals-impl o other))

             IHash
             (-hash [o] (impl-hash o))

             IIterable
             (-iterator [o] (-iterator coll))

             ISeqable
             (-seq [o] (seq coll))

             ICounted
             (-count [o] (count coll))

             ILookup
             (-lookup [o k] (get coll k))
             (-lookup [o k not-found] (get coll k not-found))

             IFind
             (-find [o k] (-find coll k))

             IKVReduce
             (-kv-reduce [o f init] (reduce-kv coll f init))

             IFn
             (-invoke [o k] (coll k))
             (-invoke [o k not-found] (coll k not-found))]))

#?(:clj
   (defmethod print-method Entity [o ^java.io.Writer w]
     (.write w (impl-print o))))

#?(:clj
   (defmethod print-dup Entity [o w]
     (print-method o w)))

(defn- equals-impl [o other]
  ; This is an identity check, because in future all value access should be reactive
  ; which means ILookup will return cursors and the call site will deref.
  ; Equality on entities is not defined because entities are refs. Databases are values.
  ; Omit tx-tempid, that would be a big problem if it wasn't the same.
  (and (instance? Entity other)
       (= (.-uri o) (.-uri other))                          ; identity
       (= (:db/id (.-coll o)) (:db/id (.-coll other)))))
