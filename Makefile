DEPLOY_ENVIRONMENT ?= prod
DEPLOY_TAG ?= $(shell git rev-parse HEAD)

.PHONY: web.watch
web.watch:
	stack build --fast --pedantic --file-watch \
	  --exec "bash -c \"pkill rcl-web; stack exec rcl-web &\"" \

.PHONY: deploy
deploy:
	platform container:login
	platform container:push --tag $(DEPLOY_TAG) --no-create
	platform deploy --environment $(DEPLOY_ENVIRONMENT) --tag $(DEPLOY_TAG)
