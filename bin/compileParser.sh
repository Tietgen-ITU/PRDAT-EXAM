#!/bin/sh
binDir=/data/bin

mono $binDir/fsyacc.exe --module $1Par $1Par.fsy
