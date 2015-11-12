(ns cad.platonic
  (:refer-clojure :exclude [import use])
  (:require [scad-clj.model :refer :all]
            [scad-clj.write :refer [write-scad]]))

; ==============================================================================
; Shared constants and functions

(defn spit-scad
  [filename object]
  (spit (str "output/platonic/" filename ".scad") (write-scad object)))

(defn cartesian-product
  ([]
   '(()))
  ([xs & more]
   (mapcat #(map (partial cons %)
                 (apply cartesian-product more))
           xs)))

(def diff-adjust 0.01) ; Small amount to make sure objects pierce correctly.

(defn smooth [r fn object]
  (->> object (minkowski (sphere r :fn fn))))

(defn offset-3d [r fn object]
  [(note "Offset 3D:") (->> object (minkowski (sphere r :fn fn)))])

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


; ==============================================================================
; Platonic Collection

