(ns cad.platonic
  (:refer-clojure :exclude [import use])
  (:require [cad.core :refer [cartesian-product fillet spit-scad]]
            [scad-clj.model :refer :all]))


; ==============================================================================
; Shared constants and functions

(def write (partial spit-scad "output/platonic/"))


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
  (write "platonic-hexahedron-test-01.low-res"
         (platonic-hexahedron-test-01 true))
  (write "platonic-hexahedron-test-01"
         (platonic-hexahedron-test-01 false))
  (write "platonic-hexahedron-test-02.low-res"
         (platonic-hexahedron-test-02 true))
  (write "platonic-hexahedron-test-02"
         (platonic-hexahedron-test-02 false))
  )
