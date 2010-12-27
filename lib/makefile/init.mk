init: static var/main.db
static:
	mkdir static
var:
	mkdir var
var/main.db: var
	touch $@
