WATCH_FILES= find . -name "*tex" -type f -print 2> /dev/null

entr_warn:
	@echo "----------------------------------------------------------"
	@echo "     ! File watching functionality non-operational !      "
	@echo "                                                          "
	@echo "Install entr(1) to automatically run tasks on file change."
	@echo "See http://entrproject.org/                               "
	@echo "----------------------------------------------------------"

build:
	pdflatex -jobname=out/new-procedure new-procedure.tex && bibtex new-procedure.tex

build-algorithm:
	pdflatex -jobname=out/algorithm algorithm.tex

watch:
	if command -v entr > /dev/null; then ${WATCH_FILES} | \
	entr -c $(MAKE) build; else $(MAKE) entr_warn; fi

watch-algorithm:
	if command -v entr > /dev/null; then ${WATCH_FILES} | \
	entr -c $(MAKE) build-algorithm; else $(MAKE) entr_warn; fi

