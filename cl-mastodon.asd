(defsystem "cl-mastodon"
  :depends-on ("dexador"
               "jonathan")
  :serial t
  :components ((:file "mastodon")))
