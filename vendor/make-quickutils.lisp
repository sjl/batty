(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :curry
               :rcurry
               :maxf
               :with-gensyms

               )
  :package "B.QUICKUTILS")
