(defsystem "cl-mastodon"
  :depends-on ("dexador"
               "jonathan"
               "alexandria")
  :serial t
  :components ((:file "util")
               (:file "config")
               (:file "entity")
               (:file "base")
               (:file "mastodon")))
