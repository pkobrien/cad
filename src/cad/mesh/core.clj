(ns cad.mesh.core
  (:refer-clojure :exclude [+ - * / == min max])
  (:require [clojure.core.matrix :refer :all]
            [clojure.core.matrix.operators :refer :all]
            [thi.ng.dstruct.core :as dc]
            [thi.ng.geom.core :as gc]
            [thi.ng.geom.core.utils :as gu]
            [thi.ng.geom.core.vector :as gv]
            [cad.mesh.protocol :as mp]
            [clojure.string :as string]))

;(set! *warn-on-reflection* true)
;(set! *unchecked-math* true)
;(set! *unchecked-math* :warn-on-boxed)

(set-current-implementation :vectorz)


; ==============================================================================
; Geometry Functions

(defn vec3
  [x y z]
  (gv/vec3 x y z))


; ==============================================================================
; Printing/Debugging Helpers

(defn prn-face-count
  ([mesh]
   (prn-face-count mesh "Mesh"))
  ([mesh msg]
   (prn (string/join " " [msg "Face-Count:" (count (mp/faces mesh))]))
   mesh))

(defn prn-sides-count
  [mesh]
  (prn (string/join " " ["Sides-Count:"
                         (into (sorted-map)
                               (frequencies (map count (mp/faces mesh))))]))
  mesh)
