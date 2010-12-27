commentdump:
	$(HOSTEXEC) sqlite3 "public_html/$(HOST)/$(DBPATH) '.dump Comment'"
commentsync:
	sqlite3 $(DBPATH) 'drop table Comment'
	make -s commentdump | sqlite3 $(DBPATH)
