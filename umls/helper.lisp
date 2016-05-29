(defun get-parent-hierarchy-for-concept (cui)
  "For the give CUI get the atoms, and from those get the parents, recursively.
   Cache results. The result would be parents up to several roots, one for 
   each difference source, apparently. For each node get the source, the id, 
   the terms, and the definitions."
  
