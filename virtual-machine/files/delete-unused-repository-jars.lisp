(loop for jar in (directory "~/.m2/**/*.jar")
      unless (find (namestring jar) abcl-asdf::*added-to-classpath* :test 'equalp :key 'namestring)
	do (delete-file jar))
