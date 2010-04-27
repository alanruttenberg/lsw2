#!/bin/sh
tag=0.9
svn copy http://svn.mumble.net:8080/svn/lsw/trunk/abcl   http://svn.mumble.net:8080/svn/lsw/tags/release-$tag/ -m "release $tag"
svn copy http://svn.mumble.net:8080/svn/lsw/trunk/bin http://svn.mumble.net:8080/svn/lsw/tags/release-$tag/ -m "release $tag"
svn copy http://svn.mumble.net:8080/svn/lsw/trunk/owl http://svn.mumble.net:8080/svn/lsw/tags/release-$tag/ -m "release $tag"
svn copy http://svn.mumble.net:8080/svn/lsw/trunk/jss  http://svn.mumble.net:8080/svn/lsw/tags/release-$tag/ -m "release $tag"
svn copy http://svn.mumble.net:8080/svn/lsw/trunk/util  http://svn.mumble.net:8080/svn/lsw/tags/release-$tag/ -m "release $tag"
svn copy http://svn.mumble.net:8080/svn/lsw/trunk/patches http://svn.mumble.net:8080/svn/lsw/tags/release-$tag/ -m "release $tag"
svn copy http://svn.mumble.net:8080/svn/lsw/trunk/images  http://svn.mumble.net:8080/svn/lsw/tags/release-$tag/ -m "release $tag"
svn copy http://svn.mumble.net:8080/svn/lsw/trunk/scripts  http://svn.mumble.net:8080/svn/lsw/tags/release-$tag/ -m "release $tag"
svn copy http://svn.mumble.net:8080/svn/lsw/trunk/extlib  http://svn.mumble.net:8080/svn/lsw/tags/release-$tag/ -m "release $tag"
svn copy http://svn.mumble.net:8080/svn/lsw/trunk/lib http://svn.mumble.net:8080/svn/lsw/tags/release-$tag/ -m "release $tag"
svn copy http://svn.mumble.net:8080/svn/lsw/trunk/ext-asdf http://svn.mumble.net:8080/svn/lsw/tags/release-$tag/ -m "release $tag"
svn copy http://svn.mumble.net:8080/svn/lsw/trunk/README.txt http://svn.mumble.net:8080/svn/lsw/tags/release-$tag/ -m "release $tag"
svn copy http://svn.mumble.net:8080/svn/lsw/trunk/start.lisp http://svn.mumble.net:8080/svn/lsw/tags/release-$tag/ -m "release $tag"

