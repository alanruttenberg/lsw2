@REM ----------------------------------------------------------------------------
@REM  Copyright 2001-2006 The Apache Software Foundation.
@REM
@REM  Licensed under the Apache License, Version 2.0 (the "License");
@REM  you may not use this file except in compliance with the License.
@REM  You may obtain a copy of the License at
@REM
@REM       http://www.apache.org/licenses/LICENSE-2.0
@REM
@REM  Unless required by applicable law or agreed to in writing, software
@REM  distributed under the License is distributed on an "AS IS" BASIS,
@REM  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
@REM  See the License for the specific language governing permissions and
@REM  limitations under the License.
@REM ----------------------------------------------------------------------------
@REM
@REM   Copyright (c) 2001-2006 The Apache Software Foundation.  All rights
@REM   reserved.

@echo off

set ERROR_CODE=0

:init
@REM Decide how to startup depending on the version of windows

@REM -- Win98ME
if NOT "%OS%"=="Windows_NT" goto Win9xArg

@REM set local scope for the variables with windows NT shell
if "%OS%"=="Windows_NT" @setlocal

@REM -- 4NT shell
if "%eval[2+2]" == "4" goto 4NTArgs

@REM -- Regular WinNT shell
set CMD_LINE_ARGS=%*
goto WinNTGetScriptDir

@REM The 4NT Shell from jp software
:4NTArgs
set CMD_LINE_ARGS=%$
goto WinNTGetScriptDir

:Win9xArg
@REM Slurp the command line arguments.  This loop allows for an unlimited number
@REM of arguments (up to the command line limit, anyway).
set CMD_LINE_ARGS=
:Win9xApp
if %1a==a goto Win9xGetScriptDir
set CMD_LINE_ARGS=%CMD_LINE_ARGS% %1
shift
goto Win9xApp

:Win9xGetScriptDir
set SAVEDIR=%CD%
%0\
cd %0\..\.. 
set BASEDIR=%CD%
cd %SAVEDIR%
set SAVE_DIR=
goto repoSetup

:WinNTGetScriptDir
set BASEDIR=%~dp0\..

:repoSetup
set REPO=


if "%JAVACMD%"=="" set JAVACMD=java

if "%REPO%"=="" set REPO=%BASEDIR%\lib

set CLASSPATH="%BASEDIR%"\etc;"%REPO%"\pellet-owlapi-ignazio1977-2.4.0-ignazio1977.jar;"%REPO%"\pellet-core-ignazio1977-2.4.0-ignazio1977.jar;"%REPO%"\aterm-java-1.8.2-p1.jar;"%REPO%"\jjtraveler-0.6.jar;"%REPO%"\shared-objects-1.4.9-p1.jar;"%REPO%"\jgrapht-jdk1.5-0.7.3.jar;"%REPO%"\xercesImpl-2.11.0.jar;"%REPO%"\xml-apis-1.4.01.jar;"%REPO%"\jena-arq-2.10.1.jar;"%REPO%"\httpclient-4.2.3.jar;"%REPO%"\httpcore-4.2.2.jar;"%REPO%"\commons-codec-1.6.jar;"%REPO%"\pellet-query-ignazio1977-2.4.0-ignazio1977.jar;"%REPO%"\antlr-runtime-3.4.jar;"%REPO%"\stringtemplate-3.2.1.jar;"%REPO%"\antlr-2.7.7.jar;"%REPO%"\owlapi-distribution-4.1.4.jar;"%REPO%"\jackson-core-2.5.1.jar;"%REPO%"\jackson-databind-2.5.1.jar;"%REPO%"\jackson-annotations-2.5.1.jar;"%REPO%"\xz-1.5.jar;"%REPO%"\sesame-model-2.7.16.jar;"%REPO%"\sesame-util-2.7.16.jar;"%REPO%"\sesame-rio-api-2.7.16.jar;"%REPO%"\sesame-rio-languages-2.7.16.jar;"%REPO%"\sesame-rio-datatypes-2.7.16.jar;"%REPO%"\sesame-rio-binary-2.7.16.jar;"%REPO%"\sesame-rio-n3-2.7.16.jar;"%REPO%"\sesame-rio-nquads-2.7.16.jar;"%REPO%"\sesame-rio-ntriples-2.7.16.jar;"%REPO%"\sesame-rio-rdfjson-2.7.16.jar;"%REPO%"\sesame-rio-rdfxml-2.7.16.jar;"%REPO%"\sesame-rio-trix-2.7.16.jar;"%REPO%"\sesame-rio-turtle-2.7.16.jar;"%REPO%"\sesame-rio-trig-2.7.16.jar;"%REPO%"\jsonld-java-sesame-0.5.0.jar;"%REPO%"\jsonld-java-0.5.0.jar;"%REPO%"\httpclient-cache-4.2.5.jar;"%REPO%"\jcl-over-slf4j-1.7.7.jar;"%REPO%"\semargl-sesame-0.6.1.jar;"%REPO%"\semargl-core-0.6.1.jar;"%REPO%"\semargl-rdfa-0.6.1.jar;"%REPO%"\semargl-rdf-0.6.1.jar;"%REPO%"\trove4j-3.0.3.jar;"%REPO%"\guava-18.0.jar;"%REPO%"\guice-4.0.jar;"%REPO%"\javax.inject-1.jar;"%REPO%"\aopalliance-1.0.jar;"%REPO%"\guice-assistedinject-4.0.jar;"%REPO%"\guice-multibindings-4.0.jar;"%REPO%"\jsr305-2.0.1.jar;"%REPO%"\commons-io-2.4.jar;"%REPO%"\pellet-pellint-ignazio1977-2.4.0-ignazio1977.jar;"%REPO%"\pellet-jena-ignazio1977-2.4.0-ignazio1977.jar;"%REPO%"\jena-core-2.10.1.jar;"%REPO%"\jena-iri-0.9.6.jar;"%REPO%"\pellet-modularity-ignazio1977-2.4.0-ignazio1977.jar;"%REPO%"\pellet-explanation-ignazio1977-2.4.0-ignazio1977.jar;"%REPO%"\slf4j-simple-1.7.7.jar;"%REPO%"\slf4j-api-1.7.7.jar;"%REPO%"\pellet-cli-ignazio1977-2.4.0-ignazio1977.jar

set ENDORSED_DIR=
if NOT "%ENDORSED_DIR%" == "" set CLASSPATH="%BASEDIR%"\%ENDORSED_DIR%\*;%CLASSPATH%

if NOT "%CLASSPATH_PREFIX%" == "" set CLASSPATH=%CLASSPATH_PREFIX%;%CLASSPATH%

@REM Reaching here means variables are defined and arguments have been captured
:endInit

%JAVACMD% %JAVA_OPTS%  -classpath %CLASSPATH% -Dapp.name="pellet" -Dapp.repo="%REPO%" -Dapp.home="%BASEDIR%" -Dbasedir="%BASEDIR%" pellet.Pellet %CMD_LINE_ARGS%
if %ERRORLEVEL% NEQ 0 goto error
goto end

:error
if "%OS%"=="Windows_NT" @endlocal
set ERROR_CODE=%ERRORLEVEL%

:end
@REM set local scope for the variables with windows NT shell
if "%OS%"=="Windows_NT" goto endNT

@REM For old DOS remove the set variables from ENV - we assume they were not set
@REM before we started - at least we don't leave any baggage around
set CMD_LINE_ARGS=
goto postExec

:endNT
@REM If error code is set to 1 then the endlocal was done already in :error.
if %ERROR_CODE% EQU 0 @endlocal


:postExec

if "%FORCE_EXIT_ON_ERROR%" == "on" (
  if %ERROR_CODE% NEQ 0 exit %ERROR_CODE%
)

exit /B %ERROR_CODE%
