(ns cad.mesh.ops
  (:require [clojure.set]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.gmesh :as gm]
            [thi.ng.geom.core.utils :as gu]
            [cad.mesh.color :as mc]
            [cad.mesh.core :as mm]))


; ==============================================================================
; General Operators

(defn rep
  "Repeat operation f on mesh n times."
  [mesh f n]
  (nth (iterate f mesh) n))

(defn tess
  "Returns a tesselated mesh."
  [mesh]
  (mm/gmesh (into [] (comp (map gu/tessellate-3) cat) (g/faces mesh))))


; ==============================================================================
; Conway Operators

(defn ambo
  "Returns mesh with new vertices added mid-edge and old vertices removed."
  [{:keys [faces vertices] :as mesh}]
  (let [verts (keys vertices)
        f-faces (map (fn [face]
                       (map gu/centroid (mm/face-vert-pairs face))) faces)
        v-faces (map (fn [vert]
                       (map gu/centroid (mm/vertex-edges mesh vert))) verts)
        faces (concat f-faces v-faces)]
    (mm/gmesh faces)))

;(defn expand
;  [mesh thickness]
;  ; Same as: Doo-Sabin subdivision
;  )

(defn kis
  "Returns mesh with each n-sided face divided into n triangles."
  ([mesh]
   (kis mesh mm/get-face-centroid))
  ([{:keys [faces] :as mesh} get-f-point]
   (let [new-face (fn [[p c n] f-point]
                    [c n f-point])
         new-faces (fn [face f-point]
                     (mapv #(new-face % f-point) (mm/face-vert-triples face)))
         subdivide (fn [face]
                     (if-let [f-point (get-f-point mesh face)]
                       (new-faces face f-point)
                       [face]))
         faces (mapcat subdivide faces)]
     (mm/gmesh faces))))

(defn ortho
  "Returns mesh with each n-sided face divided into n quadrilaterals."
  ([mesh]
   (ortho mesh mm/get-face-centroid))
  ([{:keys [faces edges] :as mesh} get-f-point]
   (let [get-e-point (fn [edge] (gu/centroid (vec edge)))
         new-face (fn [[p c n] f-point e-points]
                    [(e-points #{p c}) c (e-points #{c n}) f-point])
         new-faces (fn [face f-point e-points]
                     (mapv #(new-face % f-point e-points)
                           (mm/face-vert-triples face)))
         subdivide (fn [face e-points]
                     (if-let [f-point (get-f-point mesh face)]
                       (new-faces face f-point e-points)
                       (let [edged-f (vec (mapcat (fn [[c n]]
                                                    [c (e-points #{c n})])
                                                  (mm/face-vert-pairs face)))]
                         [edged-f])))
         e-points (into {} (map (fn [edge]
                                  [edge (get-e-point edge)]) (keys edges)))
         faces (mapcat #(subdivide % e-points) faces)]
     (mm/gmesh faces))))

;(defn truncate
;  "Returns mesh with new vertices added along edge and old vertices removed,
;   i.e. each vertex is replaced with a face."
;  [{:keys [faces edges] :as mesh} & {:keys [percent n-folds]
;                                     :or {percent 10 n-folds nil}}]
;  )


; ==============================================================================
; Other Operators

(defn colorize
  "Returns mesh with face colors, defaults to color based on face normal."
  ([mesh]
   (colorize mesh (mc/normal-abs-rgb) nil))
  ([mesh get-f-color]
   (colorize mesh get-f-color nil))
  ([{:keys [faces] :as mesh} get-f-color cb]
   (let [[mesh get-fc] (get-f-color mesh)
         fcolors (into {} (for [face faces]
                            (let [color (get-fc mesh face)
                                  color (if cb (cb color) color)]
                              [face color])))
         mesh (assoc mesh :fcolors fcolors)]
     mesh)))

(defn complexify
  "Symetrical edge smoothing while mostly maintaining bounding box dimensions."
  [{:keys [faces edges vertices] :as mesh}
   & {:keys [f-factor v-factor] :or {f-factor 0.5 v-factor 0.25}}]
  (let [mesh (mm/calc-vnp-map mesh)
        offset (fn [vert face] (g/mix vert (gu/centroid face) f-factor))
        fv-map (into {} (for [face faces]
                          [face (into {} (for [vert face]
                                           [vert (offset vert face)]))]))
        e-faces (for [edge (keys edges)]
                  (let [[v1 v2] (sort (vec edge))
                        f1 (get-in mesh [:vnp-map v1 v2 :face])
                        f2 (get-in mesh [:vnp-map v2 v1 :face])
                        va (get-in fv-map [f1 v2])
                        vb (get-in fv-map [f1 v1])
                        vc (get-in fv-map [f2 v1])
                        vd (get-in fv-map [f2 v2])]
                    [va vb vc vd]))
        f-faces (for [face faces]
                  (for [vert face]
                    (get-in fv-map [face vert])))
        v->faces (fn [vert]
                   (let [vf-verts (mapv #(get-in fv-map [% vert])
                                        (mm/vertex-faces mesh vert))
                         vf-vert (g/mix (gu/centroid vf-verts) vert v-factor)
                         vf-edges (mm/face-vert-pairs vf-verts)]
                     (mapv #(conj % vf-vert) vf-edges)))
        v-faces (mapcat v->faces (keys vertices))
        faces (concat e-faces f-faces v-faces)]
    (mm/gmesh faces)))

(defn skeletonize
  "Return mesh with all the flesh removed."
  [{:keys [faces] :as mesh}
   & {:keys [thickness get-f-factor] :or {thickness 1}}]
  (let [get-f-fact (fn [mesh face] 0.25)
        get-f-factor (or get-f-factor get-f-fact)
        vnormals (:vnormals (mm/calc-vertex-normals (tess mesh)))
        offset (fn [vert face f-factor] (g/mix vert (gu/centroid face) f-factor))
        offset-face (fn [face f-factor] (mapv #(offset % face f-factor) face))
        opposite-face (fn [outer-face thickness]
                        (vec (for [vertex (reverse outer-face)]
                               (g/+ vertex (g/* (vnormals vertex) (- thickness))))))
        new-face (fn [[c n] [c-off n-off]]
                   [c n n-off c-off])
        new-faces (fn [face face-off]
                    (mapv #(new-face %1 %2)
                          (mm/face-vert-pairs face)
                          (mm/face-vert-pairs face-off)))
        subdivide (fn [outer-f]
                    (let [inner-f (opposite-face outer-f thickness)]
                      (if-let [f-factor (get-f-factor mesh outer-f)]
                        (let [outer-off (offset-face outer-f f-factor)
                              inner-off (offset-face inner-f f-factor)]
                          (concat
                            (new-faces outer-f outer-off)
                            (new-faces inner-f inner-off)
                            (new-faces outer-off (reverse inner-off))))
                        [outer-f inner-f])))
        faces (mapcat subdivide faces)]
    (mm/gmesh faces)))


; ==============================================================================
; Catmull-Clark Subdivision Operator

(defn catmull-clark
  "Return a mesh with additional faces and edge points for a smoothing effect."
  [{:keys [faces edges vertices] :as mesh}
   & {:keys [get-f-point get-e-point get-v-point]}]
  (let [get-ep (fn [edge e-faces f-points]
                 (gu/centroid (concat (vec edge) (mapv f-points e-faces))))
        get-vp (fn [mesh vertex]
                 (let [f (gu/centroid (mapv gu/centroid
                                            (gm/vertex-faces* mesh vertex)))
                       vn (gm/vertex-neighbors* mesh vertex)
                       n (count vn)
                       r (gu/centroid (mapv #(g/mix vertex %) vn))]
                   (g/addm (g/madd r 2.0 f) (g/* vertex (- n 3)) (/ 1.0 n))))
        get-f-point (or get-f-point mm/get-face-centroid)
        get-e-point (or get-e-point get-ep)
        get-v-point (or get-v-point get-vp)
        new-face (fn [[p c n] f-point e-points]
                   [(e-points #{p c}) c (e-points #{c n}) f-point])
        new-faces (fn [face f-point e-points]
                    (mapv #(new-face % f-point e-points)
                          (mm/face-vert-triples face)))
        subdivide (fn [[face f-point] e-points]
                    (new-faces face f-point e-points))
        f-points (into {} (map (fn [face]
                                 [face (get-f-point mesh face)])
                               faces))
        e-points (into {} (map (fn [[edge e-faces]]
                                 [edge (get-e-point edge e-faces f-points)])
                               edges))
        v-points (into {} (map (fn [vertex]
                                 [vertex (get-v-point mesh vertex)])
                               (keys vertices)))
        v-replace (partial replace v-points)
        faces (->> (mapcat #(subdivide % e-points) f-points) (map v-replace))]
    (mm/gmesh faces)))


; ==============================================================================
; Not Quite Catmull-Clark Subdivision Operator

(defn not-quite-catmull-clark
  "Return a mesh with additional faces and edge points for a smoothing effect."
  [mesh]
  (let [get-e-point (fn [edge _ _] (gu/centroid (vec edge)))]
    (catmull-clark mesh :get-e-point get-e-point)))
