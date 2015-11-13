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
   [[]])
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

(defn platonic-hexahedron [size point-shape]
  (let [face-verts (fn [face] (into #{} (for [edge face vert edge] vert)))
        pin (fn [vert] (translate vert point-shape))
        verts [[1 1 1]
               [1 1 -1]
               [1 -1 -1]
               [1 -1 1]
               [-1 -1 1]
               [-1 -1 -1]
               [-1 1 -1]
               [-1 1 1]]
        verts (for [vert verts] (for [pos vert] (* size pos)))
        edges [[0 1] [1 2] [2 3] [3 0]
               [4 5] [5 6] [6 7] [7 4]
               [0 7] [6 1] [2 5] [4 3]]
        edges (for [edge edges] (for [vert edge] (nth verts vert)))
        faces [[0 1 2 3] [6 9 0 8] [4 5 6 7] [2 10 4 11] [3 11 7 8] [5 10 1 9]]
        faces (for [face faces] (for [edge face] (nth edges edge)))
        concs (for [edge edges]
                (let [triad (conj edge [0 0 0])]
                  (apply hull (for [vert triad] (pin vert)))))
        solid (apply hull (for [vert verts] (pin vert)))
        walls (for [face faces]
                (apply hull (for [vert (face-verts face)] (pin vert))))
        wires (for [edge edges]
                (apply hull (for [vert edge] (pin vert))))
        parts {:concs concs
               :solid [solid]
               :walls walls
               :wires wires}]
    parts))

(defn platonic-hexahedron-test-01 [low-res]
  (let [point-shape (sphere 1 :fn 24)
        shape (:wires (platonic-hexahedron 10 point-shape))]
    (if low-res (apply union shape)
                (let [r 0.5 fn 8 steps 3] (apply fillet r fn steps shape)))))

(defn platonic-hexahedron-test-02 [low-res]
  (let [outer (:wires (platonic-hexahedron 10 (sphere 1 :fn 24)))
        outer (if low-res (apply union outer)
                          (let [r 0.5 fn 8 steps 4]
                            (apply fillet r fn steps outer)))
        middle (:concs (platonic-hexahedron 10 (sphere 0.5 :fn 8)))
        middle (apply union middle)
        middle (difference middle (cube 8 8 8))
        center (:wires (platonic-hexahedron 4 (sphere 1 :fn 32)))
        center (apply union center)
        object (union outer middle center)]
    object))

(comment
  (spit-scad "platonic-hexahedron-test-01" (platonic-hexahedron-test-01 false))
  (spit-scad "platonic-hexahedron-test-01.low-res" (platonic-hexahedron-test-01 true))
  (spit-scad "platonic-hexahedron-test-02" (platonic-hexahedron-test-02 false))
  (spit-scad "platonic-hexahedron-test-02.low-res" (platonic-hexahedron-test-02 true))
  )
