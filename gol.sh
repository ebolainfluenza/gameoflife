#!/bin/sh

wc -l *.go *.js *.html
go clean -i
rm -rf images/*
go build -race # test, install
time go run gol.go -r 64 -c 64 -x 800 -y 800 -d 7
nimg=`ls -l images | wc -l`
echo "images created: $nimg"
