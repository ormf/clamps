all: clean html archive html/clamps-doc/updated
clean:
	rm -rf html/clamps-doc/overview
	rm -rf html/clamps-doc/clamps
	rm -rf html/clamps-doc/clamps-dict
	rm -f html/clamps-doc/index.html
	rm -rf html/clamps-doc/js
	rm -rf html/clamps-doc/css
	rm -rf html/clamps-doc/fonts

clamps-dictionary.org:
	sbcl --load generate-dictionary.lisp

html:
	mkdir -p html/
	emacs -q --eval "(defvar *clamps-doc-dir* \"`pwd`\")" \
	--batch --load generate-html.el
	cp -r img/clamps-img html/clamps-doc/clamps/img
	cp -r img/overview-img html/clamps-doc/overview/img
	cp -r img/clamps-dict-img html/clamps-doc/clamps-dict/img
	# ln -s ../../css html/clamps-doc/
	# ln -s ../../js html/clamps-doc/
	# ln -s ../../fonts html/clamps-doc/
	cp -rf css html/clamps-doc/
	cp -rf js html/clamps-doc/
	cp -rf fonts html/clamps-doc/

archive:
	tar -C html -czvf clamps-doc.tgz clamps-doc

html/clamps-doc/updated: clamps-doc.tgz

	mkdir html && cd html && rm -rf clamps-doc && tar xvf ../clamps-doc.tgz
	touch html/clamps-doc/updated

clean-doc:
	rm html/clamps-doc/updated

.PHONY: clean html clamps-dictionary.org archive

