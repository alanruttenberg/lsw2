#!/bin/sh
lc=`echo $1 | tr '[:upper:]' '[:lower:]'`
curl -s -F file=@../ontology/ontofox-inputs/$lc.txt -o ../ontology/$lc-imports.owl  http://ontofox.hegroup.org/service.php
