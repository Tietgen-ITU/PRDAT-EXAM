#!/bin/bash
binDir=/data/bin
currDir=$PWD

cd ../MicroC/
mono $binDir/fslex.exe --unicode FunLex.fsl
mono $binDir/fsyacc.exe --module FunPar FunPar.fsy

fsharpi -r $binDir/FsLexYacc.Runtime.dll Absyn.fs FunPar.fs FunLex.fs Parse.fs  
