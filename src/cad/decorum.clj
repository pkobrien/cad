(ns cad.decorum
  (:refer-clojure :exclude [import use])
  (:require [cad.core :refer [cartesian-product fillet spit-scad]]
            [scad-clj.model :refer :all]))


; ==============================================================================
; Shared constants and functions

(def write (partial spit-scad "output/decorum/"))

(def max-bounding-box-shapeways [125 125 200])

(def max-wall-thickness-shapeways 50)

(def diff-adjust 0.01) ; Small amount to make sure objects pierce correctly.

(defn clamp-to-bounding-box [x y z object]
  (->> object (intersection (cube x y z))))

(defn clamp-to-bounding-cyl [rs h object]
  (->> object (intersection (cylinder rs h))))

(defn smooth [r fn object]
  (->> object (minkowski (sphere r :fn fn))))


; ==============================================================================
; Cube Vase

(defn platonic-hexahedron [size-x size-y size-z point-shape focus focus-shape]
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
        verts (for [[x y z] verts] [(* x size-x) (* y size-y) (* z size-z)])
        base-verts (cons focus (for [[x y z] verts :when (neg? z)] [x y z]))
        edges [[0 1] [1 2] [2 3] [3 0]
               [4 5] [5 6] [6 7] [7 4]
               [0 7] [6 1] [2 5] [4 3]]
        edges (for [edge edges] (for [vert edge] (nth verts vert)))
        faces [[0 1 2 3] [6 9 0 8] [4 5 6 7] [2 10 4 11] [3 11 7 8] [5 10 1 9]]
        faces (for [face faces] (for [edge face] (nth edges edge)))
        base (apply hull (cons (translate focus focus-shape)
                               (for [vert base-verts] (pin vert))))
        concs (for [edge edges]
                (apply hull (cons (translate focus focus-shape)
                                  (for [vert edge] (pin vert)))))
        solid (apply hull (for [vert verts] (pin vert)))
        vess-edges [[0 3] [3 4] [4 7] [7 0]]
        vess-edges (for [edge vess-edges] (for [vert edge] (nth verts vert)))
        vessel (for [edge vess-edges]
                 (apply hull (cons (translate focus focus-shape)
                                   (for [vert edge] (pin vert)))))
        walls (for [face faces]
                (apply hull (for [vert (face-verts face)] (pin vert))))
        wires (for [edge edges]
                (apply hull (for [vert edge] (pin vert))))
        parts {:base base
               :concs concs
               :solid solid
               :vessel vessel
               :walls walls
               :wires wires}]
    parts))

(defn cuboid-vase-test-01 [low-res]
  (let [x 30 y 30 z 90
        focus [0 0 (- 30 z)]
        focus-shape (sphere 3 :fn 24)
        outer (:wires (platonic-hexahedron x y z (sphere 3 :fn 24) focus focus-shape))
        outer (if low-res (apply union outer)
                          (let [r 1 fn 8 steps 4]
                            (apply fillet r fn steps outer)))
        middle (:concs (platonic-hexahedron x y z (sphere 2 :fn 8) focus focus-shape))
        middle (if low-res (apply union middle)
                           (let [r 1 fn 8 steps 4]
                             (apply fillet r fn steps middle)))
        base (:base (platonic-hexahedron x y z (sphere 3 :fn 24) focus focus-shape))
        object (union outer middle base)]
    object))

;(write "decorum-cuboid-vase-test-01.low-res" (cuboid-vase-test-01 true))

(defn cuboid-vase-test-02 [low-res]
  (let [x 30 y 30 z 90
        focus [0 0 (- 30 z)]
        focus-shape (sphere 3 :fn 24)
        outer (:wires (platonic-hexahedron x y z (sphere 3 :fn 24) focus focus-shape))
        outer (if low-res (apply union outer)
                          (let [r 1 fn 8 steps 4]
                            (apply fillet r fn steps outer)))
        vessel (:vessel (platonic-hexahedron x y z (sphere 2 :fn 24) focus focus-shape))
        vessel (if low-res (apply union vessel)
                           (let [r 1 fn 8 steps 4]
                             (apply fillet r fn steps vessel)))
        base (:base (platonic-hexahedron x y z (sphere 3 :fn 24) focus focus-shape))
        inner (if low-res (union vessel base)
                           (let [r 1 fn 8 steps 4]
                             (fillet r fn steps vessel base)))
        object (union outer inner)]
    object))

;(write "decorum-cuboid-vase-test-02.low-res" (cuboid-vase-test-02 true))

(defn cuboid-vase-test-03 [low-res]
  (let [x 30 y 30 z 90
        focus [0 0 (- 30 z)]
        focus-shape (sphere 3 :fn 24)
        vessel (:vessel (platonic-hexahedron x y z (sphere 2 :fn 24) focus focus-shape))
        vessel (if low-res (apply union vessel)
                           (let [r 1 fn 8 steps 4]
                             (apply fillet r fn steps vessel)))
        base (:base (platonic-hexahedron x y z (sphere 3 :fn 24) focus focus-shape))
        inner (if low-res (union vessel base)
                          (let [r 1 fn 8 steps 4]
                            (fillet r fn steps vessel base)))
        object inner]
    object))

;(write "decorum-cuboid-vase-test-03.low-res" (cuboid-vase-test-03 true))

(defn cuboid-vase-test-04 [low-res]
  (let [x 30 y 30 z 90
        focus [0 0 (- 30 z)]
        focus-shape (sphere 3 :fn 24)
        vessel (:vessel (platonic-hexahedron x y z (sphere 2 :fn 24) focus focus-shape))
        vessel (if low-res (apply union vessel)
                           (let [r 1 fn 8 steps 4]
                             (apply fillet r fn steps vessel)))
        middle (:concs (platonic-hexahedron x y z (sphere 2 :fn 24) focus focus-shape))
        middle (if low-res (apply union middle)
                           (let [r 1 fn 8 steps 4]
                             (apply fillet r fn steps middle)))
        base (:base (platonic-hexahedron x y z (sphere 3 :fn 24) focus focus-shape))
        object (union vessel middle base)]
    object))

;(write "decorum-cuboid-vase-test-04.low-res" (cuboid-vase-test-04 true))

(defn cuboid-vase-test-05 [low-res]
  (let [x 30 y 30 z 90
        focus [-30 30 (- 30 z)]
        focus-shape (sphere 3 :fn 24)
        ;outer (vec (:wires (platonic-hexahedron x y z (sphere 3 :fn 24) focus focus-shape)))
        ;outer (concat (subvec outer 0 2) (subvec outer (inc 2)))
        ;outer (if low-res (apply union outer)
        ;                  (let [r 1 fn 8 steps 4]
        ;                    (apply fillet r fn steps outer)))
        middle (vec (:concs (platonic-hexahedron x y z (sphere 3 :fn 24) focus focus-shape)))
        middle (concat (subvec middle 0 2) (subvec middle (inc 2)))
        base (:base (platonic-hexahedron x y z (sphere 3 :fn 24) focus focus-shape))
        object (if low-res (apply union (cons base middle))
                           (let [r 1 fn 8 steps 4]
                             (apply fillet r fn steps (cons base middle))))
        ;object (union outer middle base)
        ;object (union middle base)
        ]
    object))

;(write "decorum-cuboid-vase-test-05.low-res" (cuboid-vase-test-05 true))

(defn cuboid-vase-test-06 [low-res]
  (let [x 50 y 50 z 80
        fn 32
        focus [-50 50 -80]
        focus-shape (sphere 3 :fn fn)
        vessel (:vessel (platonic-hexahedron x y z (sphere 3 :fn fn) focus focus-shape))
        walls (:walls (platonic-hexahedron x y z (sphere 3 :fn fn) focus focus-shape))
        walls [(nth walls 2) (nth walls 1)]
        object (if low-res (apply union (concat vessel walls))
                           (let [r 1 fn 16 steps 4]
                             (apply fillet r fn steps (concat vessel walls))))]
    object))

;(write "decorum-cuboid-vase-test-06.low-res" (cuboid-vase-test-06 true))

(defn cuboid-vase-test-07 [low-res]
  (let [x 50 y 50 z 80
        fn 32
        focus [-50 50 -80]
        focus-shape (sphere 3 :fn fn)
        vessel (:vessel (platonic-hexahedron x y z (sphere 3 :fn fn) focus focus-shape))
        walls (:walls (platonic-hexahedron x y z (sphere 3 :fn fn) focus focus-shape))
        walls [(nth walls 0) (nth walls 1)]
        object (if low-res (apply union (concat vessel walls))
                           (let [r 1 fn 16 steps 4]
                             (apply fillet r fn steps (concat vessel walls))))]
    object))

;(write "decorum-cuboid-vase-test-07.low-res" (cuboid-vase-test-07 true))

(comment
  (write "decorum-cuboid-vase-test-01.low-res" (cuboid-vase-test-01 true))
  (write "decorum-cuboid-vase-test-01" (cuboid-vase-test-01 false))
  (write "decorum-cuboid-vase-test-02.low-res" (cuboid-vase-test-02 true))
  (write "decorum-cuboid-vase-test-02" (cuboid-vase-test-02 false))
  (write "decorum-cuboid-vase-test-03.low-res" (cuboid-vase-test-03 true))
  (write "decorum-cuboid-vase-test-03" (cuboid-vase-test-03 false))
  (write "decorum-cuboid-vase-test-04.low-res" (cuboid-vase-test-04 true))
  (write "decorum-cuboid-vase-test-04" (cuboid-vase-test-04 false))
  (write "decorum-cuboid-vase-test-05.low-res" (cuboid-vase-test-05 true))
  (write "decorum-cuboid-vase-test-05" (cuboid-vase-test-05 false))
  (write "decorum-cuboid-vase-test-06.low-res" (cuboid-vase-test-06 true))
  (write "decorum-cuboid-vase-test-06" (cuboid-vase-test-06 false))
  (write "decorum-cuboid-vase-test-07.low-res" (cuboid-vase-test-07 true))
  (write "decorum-cuboid-vase-test-07" (cuboid-vase-test-07 false))
  )


; ==============================================================================
; Cube Bowl

(defn cube-bowl
  "Cube-shaped bowl. Defaults to largest size possible with Shapeways."
  ([wall-thickness floor-thickness]
   (cube-bowl wall-thickness floor-thickness max-bounding-box-shapeways))
  ([wall-thickness floor-thickness [x y z]]
   (let [smooth-r 2.5
         smooth-fn 16
         object-x (- x (* 2 smooth-r))
         object-y (- y (* 2 smooth-r))
         object-z (- z (* 2 smooth-r))
         core-smooth-r (* 2 smooth-r)
         core-smooth-adjust (- core-smooth-r smooth-r)
         core-x (- x (* 2 wall-thickness) (* 2 core-smooth-adjust))
         core-y (- y (* 2 wall-thickness) (* 2 core-smooth-adjust))
         core-z (+ diff-adjust (- z floor-thickness core-smooth-adjust))
         core-z-move (/ (+ diff-adjust floor-thickness core-smooth-adjust) 2)
         core (cube core-x core-y core-z)
         core (->> core (translate [0 0 core-z-move]))
         core (->> core (smooth core-smooth-r smooth-fn))
         object (cube object-x object-y object-z)
         object (-> object (difference core))
         object (->> object (smooth smooth-r smooth-fn))]
     object)))

(defn cube-bowl-test-01
  "Thick walls, thick floor."
  []
  (let [wall-thickness max-wall-thickness-shapeways
        floor-thickness max-wall-thickness-shapeways
        object (cube-bowl wall-thickness floor-thickness)]
    object))

(defn cube-bowl-test-02
  "Thin walls, medium floor."
  []
  (let [wall-thickness 10
        floor-thickness (/ max-wall-thickness-shapeways 2)
        object (cube-bowl wall-thickness floor-thickness)]
    object))

(defn cube-bowl-test-03
  "Medium walls, medium floor."
  []
  (let [wall-thickness (/ max-wall-thickness-shapeways 2)
        floor-thickness (/ max-wall-thickness-shapeways 2)
        object (cube-bowl wall-thickness floor-thickness)]
    object))

(comment
  (write "decorum-cube-bowl-test-01" (cube-bowl-test-01))
  (write "decorum-cube-bowl-test-02" (cube-bowl-test-02))
  (write "decorum-cube-bowl-test-03" (cube-bowl-test-03))
  )


; ==============================================================================
; Delta Vase

(defn delta-vase-side [r-heel z-heel r-toe x-toe z-toe r-head x-head z-head angle rotation]
  (let [heel (->> (sphere r-heel :fn 7)
                  (translate [0 0 r-heel])
                  (translate [0 0 z-heel]))
        toe (->> (sphere r-toe :fn 7)
                 (translate [x-toe 0 0])
                 (translate [(- r-toe) 0 r-toe])
                 (translate [0 0 z-toe])
                 (rotate [0 0 (+ angle (/ (- 180 angle) 2))]))
        head-1 (->> (sphere r-head :fn 9)
                    (translate [x-head 0 z-head])
                    (translate [(- r-head) 0 (- r-head)]))
        head-2 (->> (sphere r-head :fn 9)
                    (translate [x-head 0 z-head])
                    (translate [(- r-head) 0 (- r-head)])
                    (rotate [0 0 angle]))
        object (hull heel toe head-1 head-2)
        object (->> object (rotate [0 0 rotation]))]
    object))

(defn delta-vase [r-heel z-heel r-toe x-toe z-toe r-head x-head z-head sides fillet-r fillet-steps]
  (let [fillet-fn 8
        angle (/ 360 sides)
        rotations (range 0 360 angle)
        dv-side (partial delta-vase-side r-heel z-heel r-toe x-toe z-toe
                         r-head x-head z-head angle)
        sides (map dv-side rotations)
        object (if (and fillet-r fillet-steps)
                 (apply fillet fillet-r fillet-fn fillet-steps sides)
                 (apply union sides))]
    object))

(defn delta-vase-test-01 []
  (let [sides 3
        r-heel 2
        z-heel 0
        r-toe 2
        x-toe 43.75
        z-toe 0
        r-head 2
        x-head 50
        z-head 100
        fillet-r nil
        fillet-steps nil]
    (delta-vase r-heel z-heel r-toe x-toe z-toe r-head x-head z-head sides fillet-r fillet-steps)))

(defn delta-vase-test-02 []
  (let [sides 3
        r-heel 2
        z-heel 0
        r-toe 4
        x-toe 43.75
        z-toe 0
        r-head 2
        x-head 50
        z-head 100
        fillet-r nil
        fillet-steps nil]
    (delta-vase r-heel z-heel r-toe x-toe z-toe r-head x-head z-head sides fillet-r fillet-steps)))

(defn delta-vase-test-03 []
  (let [sides 4
        r-heel 2
        z-heel 2
        r-toe 4
        x-toe 25
        z-toe 0
        r-head 2
        x-head 50
        z-head 100
        fillet-r nil
        fillet-steps nil]
    (delta-vase r-heel z-heel r-toe x-toe z-toe r-head x-head z-head sides fillet-r fillet-steps)))

(defn delta-vase-test-04 []
  (let [sides 5
        r-heel 12
        z-heel 6
        r-toe 6
        x-toe 30
        z-toe 0
        r-head 2
        x-head 50
        z-head 100
        fillet-r nil
        fillet-steps nil]
    (delta-vase r-heel z-heel r-toe x-toe z-toe r-head x-head z-head sides fillet-r fillet-steps)))

(defn delta-vase-test-05 []
  (let [sides 6
        r-heel 12
        z-heel 0
        r-toe 3
        x-toe 30
        z-toe 0
        r-head 4
        x-head 50
        z-head 100
        fillet-r nil
        fillet-steps nil]
    (delta-vase r-heel z-heel r-toe x-toe z-toe r-head x-head z-head sides fillet-r fillet-steps)))

(defn delta-vase-test-06 []
  (let [sides 3
        r-heel 2
        z-heel 0
        r-toe 2
        x-toe 43.75
        z-toe 0
        r-head 2
        x-head 50
        z-head 100
        fillet-r 2
        fillet-steps 3]
    (delta-vase r-heel z-heel r-toe x-toe z-toe r-head x-head z-head sides fillet-r fillet-steps)))

(defn delta-vase-test-07 []
  (let [sides 3
        r-heel 2
        z-heel 0
        r-toe 4
        x-toe 43.75
        z-toe 0
        r-head 2
        x-head 50
        z-head 100
        fillet-r 2
        fillet-steps 3]
    (delta-vase r-heel z-heel r-toe x-toe z-toe r-head x-head z-head sides fillet-r fillet-steps)))

(defn delta-vase-test-08 []
  (let [sides 4
        r-heel 2
        z-heel 2
        r-toe 4
        x-toe 25
        z-toe 0
        r-head 2
        x-head 50
        z-head 100
        fillet-r 2
        fillet-steps 3]
    (delta-vase r-heel z-heel r-toe x-toe z-toe r-head x-head z-head sides fillet-r fillet-steps)))

(defn delta-vase-test-09 []
  (let [sides 5
        r-heel 12
        z-heel 6
        r-toe 6
        x-toe 30
        z-toe 0
        r-head 2
        x-head 50
        z-head 100
        fillet-r 2
        fillet-steps 3]
    (delta-vase r-heel z-heel r-toe x-toe z-toe r-head x-head z-head sides fillet-r fillet-steps)))

(defn delta-vase-test-10 []
  (let [sides 6
        r-heel 12
        z-heel 0
        r-toe 3
        x-toe 30
        z-toe 0
        r-head 4
        x-head 50
        z-head 100
        fillet-r 2
        fillet-steps 3]
    (delta-vase r-heel z-heel r-toe x-toe z-toe r-head x-head z-head sides fillet-r fillet-steps)))

(comment
  (write "decorum-delta-vase-test-01" (delta-vase-test-01))
  (write "decorum-delta-vase-test-02" (delta-vase-test-02))
  (write "decorum-delta-vase-test-03" (delta-vase-test-03))
  (write "decorum-delta-vase-test-04" (delta-vase-test-04))
  (write "decorum-delta-vase-test-05" (delta-vase-test-05))
  (write "decorum-delta-vase-test-06" (delta-vase-test-06))
  (write "decorum-delta-vase-test-07" (delta-vase-test-07))
  (write "decorum-delta-vase-test-08" (delta-vase-test-08))
  (write "decorum-delta-vase-test-09" (delta-vase-test-09))
  (write "decorum-delta-vase-test-10" (delta-vase-test-10))
  )


; ==============================================================================
; ???

(defn foo-test-01 []
  (let [smooth-r 2.5
        smooth-fn 16
        x (- max-wall-thickness-shapeways (* 2 smooth-r))
        y (- max-wall-thickness-shapeways (* 2 smooth-r))
        z (- (apply max max-bounding-box-shapeways) (* 2 smooth-r))
        object (cube x y z)
        ;object (->> object (smooth smooth-r smooth-fn))
        ]
    object))

(defn foo-test-02 []
  (let [smooth-r 2.5
        smooth-fn 16
        object-h (- (* 2 max-wall-thickness-shapeways) (* 2 smooth-r))
        object-r (- (/ max-wall-thickness-shapeways 2) smooth-r)
        object-fn 6
        cut-h (* 2 object-r)
        cut-r 7
        cut-fn 6
        cut-offset (- object-r (/ object-r 10))
        cut (cylinder [cut-r cut-r] cut-h :fn cut-fn)
        cuts (union (->> cut (translate [(+ cut-offset) 0 0]))
                    (->> cut (translate [(- cut-offset) 0 0]))
                    (->> cut (translate [0 (+ cut-offset) 0]))
                    (->> cut (translate [0 (- cut-offset) 0])))
        object (sphere object-r :fn object-fn)
        object (difference object cuts)
        object (->> object (resize [0 0 object-h]))
        ;object (->> object (smooth smooth-r smooth-fn))
        ]
    object))

(defn foo-test-03 []
  (let [smooth-r 2.5
        smooth-fn 16
        object-h (- (* 2 max-wall-thickness-shapeways) (* 2 smooth-r))
        object-r (- (/ max-wall-thickness-shapeways 2) smooth-r)
        object-fn 6
        cut-h (* 2 object-r)
        cut-r 7
        cut-fn 6
        cut-offset object-r
        cut (cylinder [cut-r cut-r] cut-h :fn cut-fn)
        cut (->> cut (translate [(+ cut-offset) 0 0]))
        cuts (apply union (for [angle (range 0 360 (/ 360 object-fn))]
                            (->> cut (rotate angle [0 0 1]))))
        object (sphere object-r :fn object-fn)
        object (difference object cuts)
        object (->> object (resize [0 0 object-h]))
        ;object (->> object (smooth smooth-r smooth-fn))
        ]
    object))

(comment
  (write "decorum-foo-test-01" (foo-test-01))
  (write "decorum-foo-test-02" (foo-test-02))
  (write "decorum-foo-test-03" (foo-test-03))
  )


; ==============================================================================
; ???

(defn bar-test-01 []
  (let [smooth-r 2.5
        smooth-fn 16
        [max-x max-y max-z] max-bounding-box-shapeways
        object-x (- max-x (* 2 smooth-r))
        object-y (- max-y (* 2 smooth-r))
        object-z (- max-z (* 2 smooth-r))
        wall-thickness 10
        core-x (- max-x (* 2 wall-thickness) (* 2 smooth-r))
        core-y (- max-y (* 2 wall-thickness) (* 2 smooth-r))
        core-z (- (+ 1 max-z) max-wall-thickness-shapeways)
        core (cube core-x core-y core-z)
        core (->> core (translate [0 0 (/ max-wall-thickness-shapeways 2)]))
        core (->> core (smooth (* 2 smooth-r) smooth-fn))
        object (cube object-x object-y object-z)
        object (difference object core)
        object (->> object (smooth smooth-r smooth-fn))]
    object))

(comment
  (write "decorum-bar-test-01" (bar-test-01))
  )
