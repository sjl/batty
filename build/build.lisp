(ql:quickload '(deploy batty))

(setf deploy:*info-plist-template* "build/Info.plist")

(deploy:define-library blt:bearlibterminal
  :system :cl-blt)

(deploy:define-resource-directory assets "assets/")

(sb-ext:gc :full t)
(asdf:operate :build-op :batty)
