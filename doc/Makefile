clean:
	rm -rf html/clamps-doc/overview
	rm -rf html/clamps-doc/clamps
	rm -rf html/clamps-doc/clamps-dict

clamps-dictionary.org:
	sbcl --load generate-dictionary.lisp

html:
	mkdir -p html/
	emacs -q --eval "(defvar *clamps-doc-dir* \"`pwd`\")" \
	--batch --load generate-html.el
	cp -r img/clamps-img html/clamps-doc/clamps/img
	cp -r img/overview-img html/clamps-doc/overview/img
	cp -r img/clamps-dict-img html/clamps-doc/clamps-dict/img
	cp -r css/clamps-css html/clamps-doc/clamps/css
	cp -r css/overview-css html/clamps-doc/overview/css
	cp -r css/clamps-dict-css html/clamps-doc/clamps-dict/css

archive:
	cd html
	tar czvf ../clamps-doc.tgz html/clamps-doc
	cd ..

.PHONY: clean html clamps-dictionary.org archive
