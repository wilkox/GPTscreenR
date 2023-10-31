all: manuscript.pdf arXiv_submission.tar.gz

manuscript.pdf manuscript.bbl: manuscript.tex fig_1.pdf fig_2a.pdf fig_2b.pdf fig_2c.pdf fig_3.pdf fig_4.pdf bibliography.bib
	R -e 'tinytex::pdflatex("manuscript.tex", clean = FALSE)'

fig_4.pdf: fig_4.R
	Rscript --no-save --vanilla fig_4.R

fig_%.pdf: fig_%.tex
	pdflatex fig_$*.tex; rm fig_$*.aux; rm fig_$*.log; pdfcrop --margins 10 fig_$*.pdf fig_$*_cropped.pdf; mv fig_$*_cropped.pdf fig_$*.pdf

ARXIV_FILES = manuscript.tex fig_1.pdf fig_2a.pdf fig_2b.pdf fig_2c.pdf fig_3.pdf fig_4.pdf manuscript.bbl
arXiv_submission.tar.gz: $(ARXIV_FILES)
	tar cf - $(ARXIV_FILES)| gzip -9 > arXiv_submission.tar.gz
