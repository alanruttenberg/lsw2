(defvar *quit* nil)
(let ((*load-verbose* nil)
      (*compile-verbose* nil)
      (*suppress-compiler-warnings* (not *load-verbose*)))
  (asdf::oos 'asdf::load-op 'owl :verbose nil)
  (asdf::oos 'asdf::load-op 'inspect :verbose nil)
  (start-server)
  (loop until *quit* do (sleep .5))
  (quit))



