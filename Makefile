HEROKU_APP ?= resolver-changelog

.PHONY: web.watch
web.watch:
	stack build --fast --pedantic --file-watch \
	  --exec "bash -c \"pkill rcl-web; stack exec rcl-web &\"" \

.PHONY: deploy
deploy:
	heroku container:push web --app $(HEROKU_APP)
	heroku container:release web --app $(HEROKU_APP)
