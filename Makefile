.PHONY: web.watch
web.watch:
	stack build --fast --pedantic --file-watch \
	  --exec "bash -c \"pkill rcl-web; stack exec rcl-web &\"" \
