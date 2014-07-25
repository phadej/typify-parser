.PHONY : all

all : typify.pdf

typify.tex : src/Language/Typify.lhs
	lhs2TeX $< > $@

typify.pdf : typify.tex
	pdflatex $<
