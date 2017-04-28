(defsystem "cl-mastodon"
  :depends-on ("dexador"
               "jonathan"
               "alexandria")
  :serial t
  :components ((:file "globals")
               (:file "entity")
               (:file "mastodon")))
