# cl-mastodon

## Usage
```common-lisp
(defvar *app* (make-app "https://mstdn.jp"))
(uiop:run-program (format nil "firefox '~A' &" (get-authorization-uri *app*))) ; copy authorization code
(init-access-token-with-code *app* code)

(get-current-user *app*) ; => #<ACCOUNT ...>
```

## License

MIT
