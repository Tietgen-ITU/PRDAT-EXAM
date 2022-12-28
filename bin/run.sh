#!/bin/sh
binDir=/data/bin
utilDir=/data/utils

fsharpi -r $binDir/FsLexYacc.Runtime.dll $@ $utilDir/Parse.fs $utilDir/ParseAndRun.fs $utilDir/ParseAndType.fs
