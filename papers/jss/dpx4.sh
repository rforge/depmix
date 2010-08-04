#!/bin/sh

cd ~/Documents/projects/depmixProject/codesvn/depmix/papers/jss/

R --vanilla < dpx4Sweave.R ;

# R --vanilla < dpx4SweaveNoEval.R ;

R --vanilla < dpx4Stangle.R ;


pdflatex depmixS4.tex ;

bibtex depmixS4 ;

pdflatex depmixS4.tex ;

pdflatex depmixS4.tex ;

open depmixS4.pdf ;




# R CMD Sweave file.Rnw





