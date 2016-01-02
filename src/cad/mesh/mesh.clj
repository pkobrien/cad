(ns cad.mesh.mesh
  (:require [cad.mesh.core :as mc]
            [cad.mesh.face :as mf]
            [cad.mesh.protocol :as mp]
            [cad.mesh.util :as mu]
            [cad.mesh.vert :as mv]))


; ==============================================================================
; General Functions

(defn centroid
  [mesh]
  (mc/centroid (mp/verts mesh)))


; ==============================================================================
; Private Helper Functions

(defn- edge-face-keyvals
  [face]
  (map (fn [pair]
         [(set pair) face])
       (mf/vert-pairs face)))

(defn- vert-face-keyvals
  [face]
  (map (fn [vert]
         [vert face])
       face))

(defn- vert-npf-keyvals
  [face]
  (map (fn [[p c n]]
         [c {:next n :prev p :face face}])
       (mf/vert-triples face)))


; ==============================================================================
; Indexing Functions

(defn edge-faces-map
  [mesh]
  (mu/hashmap-set (mapcat edge-face-keyvals (mp/faces mesh))))

(defn edge-set
  [mesh]
  (into #{} (mapcat mf/edges (mp/faces mesh))))

(defn face-normal-map
  [mesh]
  (mu/zipmapf mf/normal (mp/faces mesh)))

(defn vert-faces-map
  [mesh]
  (mu/hashmap-set (mapcat vert-face-keyvals (mp/faces mesh))))

(declare assoc-face-normal-map assoc-vert-faces-map)

(defn vert-normal-map
  [mesh]
  (let [mesh (assoc-face-normal-map mesh)
        mesh (assoc-vert-faces-map mesh)]
    (mu/zipmapf (partial mv/normal mesh) (mp/verts mesh))))

(defn vert-npfs-map
  [mesh]
  (mu/hashmap-set (mapcat vert-npf-keyvals (mp/faces mesh))))

(defn vert-set
  [mesh]
  (into #{} cat (mp/faces mesh)))


; ==============================================================================
; Mesh Annotation Functions

(defn mesh-assoc
  [mesh kw f]
  (if (seq (kw mesh))
    mesh
    (assoc mesh kw (f mesh))))

(defn assoc-edge-faces-map
  [mesh]
  (mesh-assoc mesh :edge-faces-map edge-faces-map))

(defn assoc-edge-set
  [mesh]
  (mesh-assoc mesh :edge-set edge-set))

(defn assoc-face-area-map
  [mesh]
  (if (seq (:face-area mesh))
    mesh
    (let [area (fn [face] [face (mf/area face)])
          area-map (into {} (map area (mp/faces mesh)))]
      (-> mesh
          (assoc-in [:face-area :map] area-map)
          (assoc-in [:face-area :min] (apply min (vals area-map)))
          (assoc-in [:face-area :max] (apply max (vals area-map)))))))

(defn assoc-face-circ-map
  [mesh]
  (if (seq (:face-circ mesh))
    mesh
    (let [circ (fn [face] [face (mf/circumference face)])
          circ-map (into {} (map circ (mp/faces mesh)))]
      (-> mesh
          (assoc-in [:face-circ :map] circ-map)
          (assoc-in [:face-circ :min] (apply min (vals circ-map)))
          (assoc-in [:face-circ :max] (apply max (vals circ-map)))))))

(defn assoc-face-dist-map
  [mesh point]
  (if (seq (:face-dist mesh))
    mesh
    (let [dist (fn [face] [face (mf/distance face point)])
          dist-map (into {} (map dist (mp/faces mesh)))]
      (-> mesh
          (assoc-in [:face-dist :map] dist-map)
          (assoc-in [:face-dist :min] (apply min (vals dist-map)))
          (assoc-in [:face-dist :max] (apply max (vals dist-map)))))))

(defn assoc-face-normal-map
  [mesh]
  (mesh-assoc mesh :face-normal-map face-normal-map))

(defn assoc-vert-faces-map
  [mesh]
  (mesh-assoc mesh :vert-faces-map vert-faces-map))

(defn assoc-vert-normal-map
  [mesh]
  (mesh-assoc mesh :vert-normal-map vert-normal-map))

(defn assoc-vert-npfs-map
  [mesh]
  (mesh-assoc mesh :vert-npfs-map vert-npfs-map))

(defn assoc-vert-next-pf-map
  [mesh]
  (let [mesh (assoc-vert-npfs-map mesh)
        vert-npfs-map (:vert-npfs-map mesh)
        verts (keys vert-npfs-map)
        next-pf (fn [{:keys [next prev face]}]
                  [next {:prev prev :face face}])
        vnpf-map (into {} (for [vert verts]
                            [vert (into {} (map next-pf
                                                (vert-npfs-map vert)))]))
        mesh (assoc mesh :vert-next-pf-map vnpf-map)]
    mesh))

(defn assoc-vert-set
  [mesh]
  (mesh-assoc mesh :vert-set vert-set))
