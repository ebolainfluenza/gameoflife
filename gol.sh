#!/bin/sh

wc -l *.go *.js *.html
go clean -i
if [ ! -d images ]; then
    mkdir images
fi
rm -rf images/*
go build -v -race # test, install
go install -race
time go run gol.go -r 64 -c 64 -x 620 -y 620 -d 7 -b 0x0 -g 0x0 -z 0xffff
numimg=`ls -l images | wc -l`
echo "$numimg images created"
