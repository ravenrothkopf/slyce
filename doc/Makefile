latex:	
	pdflatex main.tex

full: 
	pdflatex main.tex
	pdflatex main.tex
	bibtex main
	pdflatex main.tex
	bibtex main
	pdflatex main.tex

clean:
	rm -Rf main.aux
	rm -Rf main.log
	rm -Rf main.nav
	rm -Rf main.out
	rm -Rf main.pdf
	rm -Rf main.snm
	rm -Rf main.toc
	rm -Rf main.bbl
	rm -Rf main.blg
	rm -Rf main.fls
	rm -Rf main.synctex.gz
	rm -Rf main.fdb_latexmk