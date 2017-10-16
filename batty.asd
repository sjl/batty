(asdf:defsystem :batty
  :description "2D Batformer"

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"

  :depends-on (:beast
               :bordeaux-threads
               :deploy
               :black-tie
               :cl-blt
               :cl-pcg
               :iterate
               :losh)

  :defsystem-depends-on (:deploy)
  :build-operation "osx-app-deploy-op"
  :build-pathname "batty"
  :entry-point "b:main"

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components
                ((:file "main")))))

