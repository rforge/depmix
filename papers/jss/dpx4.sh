#!/bin/sh

R --vanilla < dpx4Sweave.R 

R --vanilla < dpx4Stangle.R

pdflatex dpx4Rev.tex

bibtex dpx4Rev

pdflatex dpx4Rev.tex

pdflatex dpx4Rev.tex

open dpx4Rev.pdf






