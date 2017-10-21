(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :curry
               :maxf
               :rcurry
               :read-file-into-string
               :with-gensyms

               )
  :package "B.QUICKUTILS")
