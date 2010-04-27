(let ((*load-verbose* nil)
      (*compile-verbose* nil)
      (*suppress-compiler-warnings* (not *load-verbose*)))
  (asdf::oos 'asdf::load-op 'owl :verbose nil))

(show-classtree (elt *arg* 0))
(show-propertytree (elt *arg* 0))
