.PHONY: deploy

project = css-expr

deploy:
	cd $$(mktemp -d) && \
	git clone $(CURDIR) && \
	raco pkg create $(project) && \
	mv $(project).zip $(project)$(version).zip && \
	mv $(project).zip.CHECKSUM $(project)$(version).zip.CHECKSUM && \
	rsync -av $(project)$(version).zip{,.CHECKSUM} leafac.com:leafac.com/websites/software/$(project)/
