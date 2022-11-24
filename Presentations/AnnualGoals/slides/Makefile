presentation.pdf: presentation.tex
	cd ../graphics && make
	pdflatex -synctex=1 -interaction=nonstopmode -shell-escape $< && pdflatex -synctex=1 -interaction=nonstopmode -shell-escape $@

clean:
	rm -rf auto/ *.aux *.log *.nav *.out *.pdf *.snm *.toc
	cd ../graphics && make clean

