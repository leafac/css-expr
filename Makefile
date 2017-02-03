.PHONY: documentation documentation/deploy documentation/clean clean

documentation: compiled-documentation/index.html

compiled-documentation/index.html: documentation/css-expr.scrbl
	cd documentation && raco scribble --dest ../compiled-documentation/ --dest-name index -- css-expr.scrbl

documentation/deploy: documentation
	rsync -av --delete compiled-documentation/ leafac.com:leafac.com/websites/software/css-expr/

documentation/clean:
	rm -rf compiled-documentation

clean: documentation/clean
