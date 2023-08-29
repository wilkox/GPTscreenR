all: manuscript.pdf

manuscript.pdf: manuscript.md fig_1.pdf fig_2a.pdf fig_2b.pdf fig_2c.pdf fig_3.pdf
	pandoc --citeproc manuscript.md -o manuscript.pdf

fig_%.pdf: fig_%.tex
	pdflatex fig_$*.tex; rm fig_$*.aux; rm fig_$*.log; pdfcrop --margins 10 fig_$*.pdf fig_$*_cropped.pdf; mv fig_$*_cropped.pdf fig_$*.pdf
