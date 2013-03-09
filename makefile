prepare:
	-mkdir -p build
	-mkdir -p build/doc
	-mkdir -p build/obj

clean:
	rm -rf build tags

doc: prepare
	pdflatex --output-directory build/doc docs/informe.tex
	pdflatex --output-directory build/doc docs/informe.tex
	pdflatex --output-directory build/doc docs/informe.tex

doc-spell:
	aspell -t check docs/informe.tex -d es
