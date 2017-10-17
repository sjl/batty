(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :curry
               :rcurry
               :with-gensyms

               )
  :package "B.QUICKUTILS")
