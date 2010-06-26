(ns #^{:author "Yacin Nadji <yacin@gatech.edu>"}
  lol.utils
  (:use yacin.onlisp.utils))

(defmacro nlet
  "Scheme-style named let macro"
  [n letargs & body]
  `(letfn [(~n ~(vec (map first letargs))
	       ~@body)]
     (~n ~@(map second letargs))))

(defn- sharp-symbol?
  "Determins if a particular symbol is a 'sym#' symbol"
  [s]
  (and (symbol? s)
       (> (count (name s)) 2)
       (.endsWith (name s) "#")))

(defn- g!-symbol?
  "Determines if a particular symbol is a 'g!' symbol"
  [s]
  (and (symbol? s)
       (> (count (name s)) 2)
       (or (.startsWith (name s) "g!")
	   (.startsWith (name s) "G!"))))

(defn- o!-symbol?
  "Determines if a particular symbol is an 'o!' symbol"
  [s]
  (and (symbol? s)
       (> (count (name s)) 2)
       (or (.startsWith (name s) "o!")
	   (.startsWith (name s) "O!"))))

(defn- o!-symbol-to-sharp-symbol
  "Returns matching sharp-symbol given o!-symbol s"
  [s]
;  (symb (.substring (name s) 2) "#"))
  (symbol (str (.substring (name s) 2) "#")))

(defmacro nif [expr pos zero neg]
  `(let [result# ~expr]
     (cond (pos? result#) ~pos
	   (zero? result#) ~zero
	   :else ~neg)))

(defmacro! square [o!x]
  `(x# x#))

(defmacro square [o!x]
  `(let [x# ~o!x]
     (* x# x#)))

(defmacro defmacro!
  "Automatic once-only macro definer"
  [name args & body]
  (let [os (filter o!-symbol? args)
	ss (map o!-symbol-to-sharp-symbol os)]
    `(defmacro ~name ~args
       (let ~(vec (interleave ss os))
	  ~@body))))

(defmacro defmacro-g!
  [name args & body]
  (let [syms (distinct (filter g!-symbol? (flatten body)))]
    `(defmacro ~name ~args
       (let ~(vec (interleave syms (map (fn [s] `(gensym ~(name s))) syms)))
	 ~@body))))

(defmacro defmacro-book!
  [name args & body]
  (let [os (filter o!-symbol? args)
	ss (map o!-symbol-to-sharp-symbol os)]
    `(defmacro-g! ~name ~args
       `(let ~(vec (interleave ss os))
	  ~(do ~@body)))))
