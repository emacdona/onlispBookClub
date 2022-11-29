all: presentation.tar.gz

logo.png:
	wget https://solutionstreet.com/img/logo.png

presentation.tar.gz: archive-index.txt handout.pdf slidesWithNotes.pdf slidesWithoutNotes.pdf
	rm -rf target
	mkdir -p target/presentation
	cp $^ target/presentation
	tar cfz $@ -C target presentation

handout.pdf: handout.tex presentation.tex logo.png
	cd ../graphics && make
	pdflatex -synctex=1 -interaction=nonstopmode -shell-escape $< \
		&& pdflatex -synctex=1 -interaction=nonstopmode -shell-escape $<

slidesWithNotes.pdf: slidesWithNotes.tex presentation.tex logo.png
	cd ../graphics && make
	pdflatex -synctex=1 -interaction=nonstopmode -shell-escape $< \
		&& pdflatex -synctex=1 -interaction=nonstopmode -shell-escape $<

slidesWithoutNotes.pdf: slidesWithoutNotes.tex presentation.tex logo.png
	cd ../graphics && make
	pdflatex -synctex=1 -interaction=nonstopmode -shell-escape $< \
		&& pdflatex -synctex=1 -interaction=nonstopmode -shell-escape $<

clean:
	rm -rf auto/ *.aux *.log *.nav *.out *.pdf *.snm *.toc \
			*.gz target _minted-* *.vrb
	cd ../graphics && make clean

