#!/bin/sh

wc -l *.go *.js *.html
go clean -i
rm -rf images/*
mkdir images
go test -race
go build -race
go install -race
time go run hlife3e.go -r 64 -c 64 -x 620 -y 620

