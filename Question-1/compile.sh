#!/bin/bash
binDir=/data/bin
currDir=$PWD

cd ../MicroC/
$binDir/compileLexer.sh C
$binDir/compileParser.sh C
$binDir/compileMachine.sh

cp Absyn.fs CPar.fs CLex.fs Parse.fs Machine.fs Comp.fs ParseAndComp.fs $currDir 
cp Machine.class $currDir 
cd $currDir 

 fsharpi -r $binDir/FsLexYacc.Runtime.dll Absyn.fs CPar.fs CLex.fs Parse.fs Machine.fs Comp.fs ParseAndComp.fs   
