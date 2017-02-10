.PHONY: deploy

project = css-expr

deploy:
	temporary_directory=$$(mktemp -d) && \
	git archive --prefix=$(project)/ $(or $(version), HEAD) | (cd $$temporary_directory && tar xf -) && \
	cd $$temporary_directory && \
	raco pkg create $(project) && \
	mv $(project).zip $(project)$(and $(version), -$(version)).zip && \
	mv $(project).zip.CHECKSUM $(project)$(and $(version), -$(version)).zip.CHECKSUM && \
	rsync -av $(project)$(and $(version), -$(version)).zip{,.CHECKSUM} leafac.com:leafac.com/websites/software/$(project)/
