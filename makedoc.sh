#!/bin/sh

FILE=$1
TEX="${1%.ml}"".tex"

ocamlweb --header --no-index $FILE -o $TEX
sed -i 's/latin1/utf8/' $TEX
pdflatex $TEX

