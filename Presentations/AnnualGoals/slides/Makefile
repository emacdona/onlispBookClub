all: handout.pdf presentation.pdf presentationWithoutNotes.pdf

logo.png:
	wget https://solutionstreet.com/img/logo.png

handout.pdf: handout.tex presentation.tex logo.png
	cd ../graphics && make
	pdflatex -synctex=1 -interaction=nonstopmode -shell-escape $< \
		&& pdflatex -synctex=1 -interaction=nonstopmode -shell-escape $<

presentation.pdf: slidesWithNotes.tex presentation.tex logo.png
	cd ../graphics && make
	pdflatex -synctex=1 -interaction=nonstopmode -shell-escape $< \
		&& pdflatex -synctex=1 -interaction=nonstopmode -shell-escape $<

presentationWithoutNotes.pdf: slidesWithoutNotes.tex presentation.tex logo.png
	cd ../graphics && make
	pdflatex -synctex=1 -interaction=nonstopmode -shell-escape $< \
		&& pdflatex -synctex=1 -interaction=nonstopmode -shell-escape $<

clean:
	rm -rf auto/ *.aux *.log *.nav *.out *.pdf *.snm *.toc
	cd ../graphics && make clean

