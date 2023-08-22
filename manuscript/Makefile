all: manuscript.pdf fig_1.pdf fig_2a.pdf

manuscript.pdf: manuscript.md
	pandoc manuscript.md -o manuscript.pdf

fig_%.pdf: fig_%.tex
	pdflatex $*.tex; rm $*.aux; rm $*.log; pdfcrop --margins 10 $*.pdf $*_cropped.pdf; mv $*_cropped.pdf $*.pdf
