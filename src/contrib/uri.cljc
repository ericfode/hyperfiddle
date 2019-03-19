(ns contrib.uri)


(defn- impl-print ^String [s-rep]
  (str "#uri " (pr-str s-rep)))

#?(:cljs
   (deftype URI [uri-str]
     Object (toString [_] uri-str)
     IPrintWithWriter (-pr-writer [o writer _] (-write writer (impl-print uri-str)))
     IComparable (-compare [this other]
                   (and (instance? URI other)
                        (compare (.-uri-str this) (.-uri-str other))))
     IHash (-hash [this] (hash uri-str))
     IEquiv (-equiv [this other]
              (and (instance? URI other)
                   (= (.-uri-str this) (.-uri-str other))))))

#?(:clj
   (defn ->URI [uri-str] (java.net.URI. uri-str)))

#?(:clj
   (defmethod print-method java.net.URI [o ^java.io.Writer w]
     (.write w (impl-print (str o)))))

#?(:clj
   (defmethod print-dup java.net.URI [o ^java.io.Writer w]
     (.write w (impl-print (str o)))))

(def read-URI #(->URI %))

(defn is-uri? [o] #?(:cljs (instance? URI o)
                     :clj  (instance? java.net.URI o)))
