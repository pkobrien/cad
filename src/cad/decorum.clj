(ns cad.decorum
  (:refer-clojure :exclude [import use])
  (:require [scad-clj.model :refer :all]
            [scad-clj.write :refer [write-scad]]))

; ==============================================================================
; Shared constants and functions

(defn spit-scad
  [filename object]
  (spit (str "output/decorum/" filename ".scad") (write-scad object)))

(defn cartesian-product
  ([]
   '(()))
  ([xs & more]
   (mapcat #(map (partial cons %)
                 (apply cartesian-product more))
           xs)))

(def max-bounding-box-shapeways [125 125 200])

(def max-wall-thickness-shapeways 50)

(def diff-adjust 0.01) ; Small amount to make sure objects pierce correctly.

(defn clamp-to-bounding-box [x y z object]
  (->> object (intersection (cube x y z))))

(defn clamp-to-bounding-cyl [rs h object]
  (->> object (intersection (cylinder rs h))))

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
  (spit-scad "decorum-cube-bowl-test-01" (cube-bowl-test-01))
  (spit-scad "decorum-cube-bowl-test-02" (cube-bowl-test-02))
  (spit-scad "decorum-cube-bowl-test-03" (cube-bowl-test-03))
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
  (spit-scad "decorum-delta-vase-test-01" (delta-vase-test-01))
  (spit-scad "decorum-delta-vase-test-02" (delta-vase-test-02))
  (spit-scad "decorum-delta-vase-test-03" (delta-vase-test-03))
  (spit-scad "decorum-delta-vase-test-04" (delta-vase-test-04))
  (spit-scad "decorum-delta-vase-test-05" (delta-vase-test-05))
  (spit-scad "decorum-delta-vase-test-06" (delta-vase-test-06))
  (spit-scad "decorum-delta-vase-test-07" (delta-vase-test-07))
  (spit-scad "decorum-delta-vase-test-08" (delta-vase-test-08))
  (spit-scad "decorum-delta-vase-test-09" (delta-vase-test-09))
  (spit-scad "decorum-delta-vase-test-10" (delta-vase-test-10))
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
  (spit-scad "decorum-foo-test-01" (foo-test-01))
  (spit-scad "decorum-foo-test-02" (foo-test-02))
  (spit-scad "decorum-foo-test-03" (foo-test-03))
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
  (spit-scad "decorum-bar-test-01" (bar-test-01))
  )
