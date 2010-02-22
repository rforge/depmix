#!/bin/sh


cd ~/Documents/projects/depmixProject/codesvn/depmix/papers/jss/

pdflatex article.tex
bibtex article
pdflatex article.tex
pdflatex article.tex

open article.pdf




