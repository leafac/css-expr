.PHONY: deploy

project = css-expr

deploy:
	temporary_directory=$$(mktemp -d) && \
	git archive --prefix=$(project)/ HEAD | (cd $$temporary_directory && tar xf -) && \
	cd $$temporary_directory && \
	raco pkg create $(project) && \
	rsync -av $(project).zip{,.CHECKSUM} leafac.com:leafac.com/websites/software/$(project)/
