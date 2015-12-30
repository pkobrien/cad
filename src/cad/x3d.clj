(ns cad.x3d
  (:require [thi.ng.geom.core :as gc]
            [cad.mesh.core :as mm]
            [clojure.string :as string]
            [clojure.data.xml :as xml]))


; ==============================================================================
; X3D Output

(defn xml-emit
  "Prints the given Element tree as XML text to stream.
   Options:
    :encoding <str>          Character encoding to use
    :doctype  <str>          Document type (DOCTYPE) declaration to use"
  [e ^java.io.Writer stream & {:keys [encoding doctype] :or {encoding "UTF-8"}}]
  (let [^javax.xml.stream.XMLStreamWriter writer (-> (javax.xml.stream.XMLOutputFactory/newInstance)
                                                     (.createXMLStreamWriter stream))]
    (when (instance? java.io.OutputStreamWriter stream)
      (xml/check-stream-encoding stream encoding))
    (.writeStartDocument writer encoding "1.0")
    (when doctype
      (.writeDTD writer doctype))
    (doseq [event (xml/flatten-elements [e])]
      (xml/emit-event event writer))
    (.writeEndDocument writer)
    stream))

(defn xml-emit-indented
  "Emits the XML and indents the result.  WARNING: this is slow
   it will emit the XML and read it in again to indent it.  Intended for
   debugging/testing only."
  [e ^java.io.Writer stream & {:as opts}]
  (let [sw (java.io.StringWriter.)
        _ (apply xml-emit e sw (apply concat opts))
        source (-> sw .toString java.io.StringReader. javax.xml.transform.stream.StreamSource.)
        result (javax.xml.transform.stream.StreamResult. stream)]
    (.transform (xml/indenting-transformer) source result)))

(defn write-x3d
  "Writes the given mesh as X3D XML to output stream wrapper."
  [out mesh & {:keys [indent? units meta] :or {indent? false}}]
  (let [faces (gc/faces mesh)
        verts (mm/mesh-vert-set mesh)
        face-color-map (:face-color-map mesh)
        face-colors (vec (set (vals face-color-map)))
        face-normal-map (mm/mesh-face-normal-map mesh)
        face-normals (vec (set (vals face-normal-map)))
        vindex (zipmap verts (range))
        cindex (zipmap face-colors (range))
        nindex (zipmap face-normals (range))
        get-cindex (fn [face] (cindex (face-color-map face)))
        get-nindex (fn [face] (nindex (face-normal-map face)))
        per-vert-format (fn [coll] (str (string/join " -1 " coll) " -1"))
        list-format (fn [coll] (string/join " " (apply concat coll)))
        vi-face-format (fn [face] (string/join " " (mapv vindex face)))
        formatted-vertex-index (per-vert-format (map vi-face-format faces))
        formatted-color-index (string/join " " (map get-cindex faces))
        formatted-normal-index (string/join " " (map get-nindex faces))
        formatted-vertex-list (list-format verts)
        formatted-color-list (list-format face-colors)
        formatted-normal-list (list-format face-normals)
        contents (xml/sexp-as-element
                   [:X3D {:version "3.3" :profile "Immersive"}
                    (when (or (seq units) (seq meta))
                      [:head
                       (for [unit units]
                         [:unit unit])
                       (for [[k v] meta]
                         [:meta {:name (name k) :content (str v)}])])
                    [:Scene
                     [:Shape
                      [:IndexedFaceSet {:solid "true"
                                        :ccw "true"
                                        :colorPerVertex "false"
                                        :convex "true"
                                        :creaseAngle "0"
                                        :normalPerVertex "false"
                                        :coordIndex formatted-vertex-index
                                        :colorIndex formatted-color-index
                                        :normalIndex formatted-normal-index}
                       [:Coordinate {:point formatted-vertex-list}]
                       [:ColorRGBA {:color formatted-color-list}]
                       [:Normal {:vector formatted-normal-list}]
                       ]]]])
        doctype "<!DOCTYPE X3D PUBLIC \"ISO//Web3D//DTD X3D 3.3//EN\" \"http://www.web3d.org/specifications/x3d-3.3.dtd\">"
        emit (if indent? xml-emit-indented xml-emit)]
    (emit contents out :doctype doctype)
    out))
