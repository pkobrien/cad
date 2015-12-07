(ns cad.core
  (:refer-clojure :exclude [import use])
  (:require [scad-clj.model :refer :all]
            [scad-clj.write :refer [write-scad]]
            [thi.ng.geom.core :as g]
            [clojure.java.io :as io]
            [thi.ng.geom.mesh.io :as mio]
            [clj-time.format :as tf]
            [clj-time.core :as time]
            [cad.x3d :as x3d]))


(defn cartesian-product
  ([]
   '(()))
  ([xs & more]
   (mapcat #(map (partial cons %) (apply cartesian-product more)) xs)))


; ==============================================================================
; Thi.ng

(defn save-stl
  [path mesh]
  (with-open [out (io/output-stream path)]
    (mio/write-stl
      (mio/wrapped-output-stream out)
      (g/tessellate mesh))))

(defn save-x3d
  [path mesh & {:keys [indent?] :or {indent? false}}]
  (let [now (time/now)
        date (tf/unparse (tf/formatters :rfc822) now)
        year (tf/unparse (tf/formatters :year) now)
        copy (str "Copyright " year " Patrick K. O'Brien")
        meta (array-map
               :creator "Patrick K. O'Brien"
               :created date
               :copyright copy
               :generator "Custom Clojure Code")
        units [(array-map
                 :category "length"
                 :name "millimeters"
                 :conversionFactor "0.001")]]
    (with-open [out (io/writer path)]
      (x3d/write-x3d out mesh :indent? indent? :meta meta))))


; ==============================================================================
; OpenSCAD

(defn spit-scad
  [path filename object]
  (spit (str path filename ".scad") (write-scad object)))

(defn offset-3d [r fn object]
  [(note "Offset 3D:") (minkowski (sphere r :fn fn) object)])

(defn fillet [r fn steps & children] ; fn should be a multiple of 4
  (let [pieces (for [[i1 child1] (map-indexed vector children)
                     [i2 child2] (map-indexed vector children)
                     :when (< i1 i2)]
                 (let [inter1 (intersection child1 child2)]
                   (for [step (range 1 (inc steps))]
                     (let [r2 (* r (/ step steps))
                           r3 (* r (/ (+ 1 (- steps step)) steps))
                           offset1 (offset-3d r2 fn inter1)
                           offset2 (offset-3d r3 fn inter1)
                           inter2 (render (intersection child1 offset1))
                           inter3 (render (intersection child2 offset2))]
                       (hull inter2 inter3)))))]
    (union (apply union children)
           (note "Fillets:")
           (apply union pieces))))
