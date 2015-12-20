(defproject cad "0.1.0-SNAPSHOT"
  :description "3D Printable Designs using Clojure"
  :url "https://github.com/pkobrien/cad"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/data.xml "0.0.8"]
                 [bardo "0.1.2-SNAPSHOT"]
                 [net.mikera/clisk "0.11.0"]
                 [clj-time "0.11.0"]
                 [scad-clj "0.6.0-SNAPSHOT"]
                 [thi.ng/color "1.0.0"]
                 [thi.ng/geom "0.0.908"]
                 [thi.ng/math "0.1.4"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
