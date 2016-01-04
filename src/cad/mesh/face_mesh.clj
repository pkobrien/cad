(ns cad.mesh.face-mesh
  (:require [cad.mesh.face :as mf]
            [cad.mesh.mesh :as mm]
            [cad.mesh.protocol :as mp]
            [clojure.core.matrix :as mx])
  (:import (cad.mesh.protocol IPolygonMesh)))


(defrecord FaceMesh [face-set]
  IPolygonMesh
  (faces [m] (:face-set m))
  (edges [m] (or (:edge-set m) (keys (:edge-faces-map m)) (mm/edge-set m)))
  (verts [m] (or (:vert-set m) (keys (:vert-npfs-map m)) (mm/vert-set m)))
  (assoc-edge-faces-map [m] (mm/assoc-edge-faces-map m))
  (assoc-face-area-map [m] (mm/assoc-face-area-map m))
  (assoc-face-circ-map [m] (mm/assoc-face-circ-map m))
  (assoc-face-dist-map [m point] (mm/assoc-face-dist-map m point))
  (assoc-vert-npfs-map [m] (mm/assoc-vert-npfs-map m))
  (centroid [m] (mm/centroid m))
  (edge-faces-map [m] (or (:edge-faces-map m) (mm/edge-faces-map m)))
  (face-color-map [m] (:face-color-map m))
  (face-normal-map [m] (or (:face-normal-map m) (mm/face-normal-map m)))
  (scale [m factor] (mx/scale (vec (mp/verts m)) factor) m)
  (scale! [m factor] (mx/scale! (vec (mp/verts m)) factor) m)
  (vert-faces-map [m] (or (:vert-faces-map m) (mm/vert-faces-map m)))
  (vert-normal-map [m] (or (:vert-normal-map m) (mm/vert-normal-map m)))
  (vert-npfs-map [m] (or (:vert-npfs-map m) (mm/vert-npfs-map m))))


(defn fmesh
  ([]
   (let [face-set #{}]
     (->FaceMesh face-set)))
  ([faces]
   (let [face-set (set (filter mf/unique-verts? faces))]
     (->FaceMesh face-set))))
