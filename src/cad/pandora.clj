(ns cad.pandora
  (:refer-clojure :exclude [import use])
  (:require [scad-clj.model :refer :all]
            [scad-clj.write :refer [write-scad]]))

; ==============================================================================
; Shared constants and functions

(defn spit-scad
  [filename object]
  (spit (str "output/pandora/" filename ".scad") (write-scad object)))


(defn ball [radius faces]
  (->> (sphere radius)
       (with-fn faces)
       (translate [radius radius radius])))

(defn block [size]
  (cube size size size :center false))

(defn position [spacing size n]
  (let [offset (/ (- size spacing) 2)]
    (- (* n spacing) offset)))

(defn grid-3d [nx ny nz]
  (for [z (range nz)
        y (range ny)
        x (range nx)]
    [x y z]))

(defn make-ball [spacing radius i [x y z]]
  (let [faces (+ 5 i)
        size (* 2 radius)
        pos (partial position spacing size)]
    (->> (ball radius faces)
         (translate [(pos x) (pos y) (pos z)]))))

(defn make-block [x y z spacing size]
  (let [pos (partial position spacing size)]
    (->> (block size)
         (translate [(pos x) (pos y) (pos z)]))))

(defn ball-grid-3d
  [grid spacing radius]
  (let [ball-maker (partial make-ball spacing radius)]
    (map-indexed ball-maker grid)))

(defn ball-box [spacing radius shrinkage]
  (let [nx 3 ny 3 nz 3
        dim (fn [n] (- (* (dec n) spacing) (* 2 shrinkage)))
        pos #(+ (/ spacing 2) shrinkage)
        radius (- radius shrinkage)]
    (union
      (->> (cube (dim nx) (dim ny) (dim nz) :center false)
           (translate [(pos) (pos) (pos)]))
      (ball-grid-3d (filter #(not= [1 1 1] %) (grid-3d nx ny nz)) spacing radius))))

(defn outer-box [spacing radius]
  (ball-box spacing radius 0))

(defn inner-box [spacing radius]
  (let [shrinkage 0.5]
    (ball-box spacing radius shrinkage)))

(defn pandora-test-01 []
  (let [spacing 10
        radius 4.5
        height (* 1 spacing)
        hole (->> (cylinder (/ spacing 2) height :center false)
                  (with-fn 180)
                  (translate [(* 1.5 spacing) (* 1.5 spacing) 0]))]
    (difference
      (outer-box spacing radius)
      (inner-box spacing radius)
      hole)))

;(spit "output/pandora-temp.scad" (write-scad (inner-box 10 4.5)))

(comment
  (spit-scad "pandora-test-01" (pandora-test-01))
  )
