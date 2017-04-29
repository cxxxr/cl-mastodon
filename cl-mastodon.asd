(defsystem "cl-mastodon"
  :depends-on ("dexador"
               "jonathan"
               "alexandria")
  :serial t
  :components ((:file "config")
               (:file "entity")
               (:file "base")
               (:file "accounts")
               (:file "mastodon")))
