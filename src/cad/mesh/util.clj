(ns cad.mesh.util
  (:require [thi.ng.color.core :as col]
            [cad.mesh.protocol :as mp]
            [clojure.string :as string]))


; ==============================================================================
; Shared Constants

(def ^:const PI Math/PI)
(def ^:const TAU (* PI 2.0))

(def ^:const THREE-HALVES-PI (* PI 1.5))

(def ^:const HALF-PI (/ PI 2.0))
(def ^:const THIRD-PI (/ PI 3.0))
(def ^:const QUARTER-PI (/ PI 4.0))
(def ^:const SIXTH-PI (/ PI 6.0))

(def ^:const DEG (/ 180.0 PI))
(def ^:const RAD (/ PI 180.0))

(def ^:const PHI (/ (inc (Math/sqrt 5.0)) 2))
(def ^:const SQRT2 (Math/sqrt 2))
(def ^:const SQRT3 (Math/sqrt 3))


; ==============================================================================
; Helper Functions

(defn abs [x]
  (Math/abs x))

(defn abs-zero
  [x]
  (if (zero? x) 0.0 x))

(defn degrees [theta] (* (double theta) DEG))

(defn radians [theta] (* (double theta) RAD))

(defn clamp [min max x]
  (let [x (long x) min (long min) max (long max)]
    (if (< x min) min (if (> x max) max x))))

(defn clamp-normalized [x]
  (let [x (double x)] (if (< x -1.0) -1.0 (if (> x 1.0) 1.0 x))))

(defn round2
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(def round2safe (partial round2 14))

(defn hashmap-set
  [keyvals]
  (persistent!
    (reduce
      (fn [ret [k v]]
        (assoc! ret k (conj (get ret k #{}) v)))
      (transient {}) keyvals)))

(defn zipmapf [f coll]
  (zipmap coll (map f coll)))

(defmacro spy [x]
  `(let [x# ~x]
     (println "<=" '~x "=>")
     (println x#)
     x#))


; ==============================================================================
; Color Functions

(defn color-mod
  "Returns a function that will modify a color."
  [f]
  (fn [rgba]
    @(f (col/rgba rgba))))


; ==============================================================================
; Printing/Debugging Helpers

(defn prn-face-count
  ([mesh]
   (prn-face-count mesh "Mesh"))
  ([mesh msg]
   (prn (string/join " " [msg "Face-Count:" (count (mp/faces mesh))]))
   mesh))

(defn prn-sides-count
  [mesh]
  (prn (string/join " " ["Sides-Count:"
                         (into (sorted-map)
                               (frequencies (map count (mp/faces mesh))))]))
  mesh)
