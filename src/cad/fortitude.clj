(ns cad.fortitude
  (:refer-clojure :exclude [import use])
  (:require [scad-clj.model :refer :all]
            [scad-clj.write :refer [write-scad]]))

; ==============================================================================
; Shared constants and functions

(defn spit-scad
  [filename object]
  (spit (str "output/fortitude/" filename ".scad") (write-scad object)))

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
; Fortitude Collection

(defn fortitude-01 []
  (let [point-shape (sphere 1 :fn 24)
        locs (cartesian-product [10 -10] [5 -5] [15 -15])]
    (for [[loc1 loc2] (partition 2 1 (cons (last locs) locs))]
      (let [p1 (translate loc1 point-shape)
            p2 (translate loc2 point-shape)
            rod (hull p1 p2)]
        rod))))

(defn fortitude-test-01 [rough]
  (if rough
    (apply union (fortitude-01))
    (let [r 1
          fn 8
          steps 5]
      (apply fillet r fn steps (fortitude-01)))))

(defn fortitude-02 []
  (let [point-shape (sphere 1 :fn 4)
        locs (cartesian-product [10 -10] [5 -5] [15 -15])]
    (for [[loc1 loc2] (partition 2 1 (cons (last locs) locs))]
      (let [p1 (translate loc1 point-shape)
            p2 (translate loc2 point-shape)
            rod (hull p1 p2)]
        rod))))

(defn fortitude-test-02 [rough]
  (if rough
    (apply union (fortitude-02))
    (let [r 1
          fn 16
          steps 7]
      (apply fillet r fn steps (fortitude-02)))))

(defn fortitude-03 []
  (let [point-shape (cube 1 1 1)
        positions (cartesian-product [10 -10] [10 -10] [10 -10])
        position-pairs (cartesian-product positions positions)
        center (cube 10 10 10)]
    (concat [center]
            (for [position positions]
              (translate position (sphere 2 :fn 4)))
            (for [[position1 position2] position-pairs]
              (let [p1 (translate position1 point-shape)
                    p2 (translate position2 point-shape)
                    bar (hull p1 p2)]
                bar)))))

(defn fortitude-test-03 [rough]
  (let [parts (fortitude-03)
        object (if rough (apply union parts)
                         (let [r 2
                               fn 4
                               steps 2]
                           (apply fillet r fn steps parts)))
        object (difference object
                           (cube 8 8 8)
                           (cube 2 11 11)
                           (cube 11 2 11)
                           (cube 11 11 2))]
    object))

;(spit-scad "fortitude-test-03" (fortitude-test-03 true))

(defn fortitude-04 []
  (let [point-shape (cube 1 1 1)
        positions (cartesian-product [20 -20] [20 -20] [20 -20])
        position-pairs (cartesian-product positions positions)]
    (concat [#_(cube 4 4 4)
             (difference (cube 8 8 8)
                         #_(->> (cube 4 4 10) (rotate [90 0 0]))
                         #_(->> (cube 4 4 10) (rotate [0 90 0]))
                         (->> (cylinder 3 10 :fn 8) (rotate [90 0 0]))
                         (->> (cylinder 3 10 :fn 8) (rotate [0 90 0]))
                         #_(sphere 4 :fn 8)
                         #_(cylinder 3 10 :fn 8))]
            (for [position positions]
              (translate position (sphere 2 :fn 4)))
            (for [[position1 position2] position-pairs]
              (let [p1 (translate position1 point-shape)
                    p2 (translate position2 point-shape)
                    bar (hull p1 p2)]
                bar)))))

(defn fortitude-test-04 [rough]
  (let [shape (fortitude-04)]
    (if rough (apply union shape)
              (let [r 2
                    fn 4
                    steps 2]
                (apply fillet r fn steps shape)))))

;(spit-scad "fortitude-test-04" (fortitude-test-04 true))

(comment
  (spit-scad "fortitude-test-01" (fortitude-test-01 false))
  (spit-scad "fortitude-test-02" (fortitude-test-02 false))
  (spit-scad "fortitude-test-03" (fortitude-test-03 false))
  (spit-scad "fortitude-test-04" (fortitude-test-04 false))
  )
