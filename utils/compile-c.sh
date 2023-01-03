#!/bin/bash
binDir=/data/bin
currDir=$PWD

mono $binDir/fslex.exe --unicode CLex.fsl
mono $binDir/fsyacc.exe --module CPar CPar.fsy
javac Machine.java

fsharpi -r $binDir/FsLexYacc.Runtime.dll Absyn.fs CPar.fs CLex.fs Parse.fs Machine.fs Comp.fs ParseAndComp.fs   
