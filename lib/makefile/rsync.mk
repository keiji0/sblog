RSYNC = rsync
RSYNC_EXCLUDE_LIST = lib/excludelist
_RSYNC_OPTION = -avz --delete --exclude-from=$(RSYNC_EXCLUDE_LIST) -e ssh
RSYNC_OPTION = 

rsync: commentsync
	rsync $(_RSYNC_OPTION) $(RSYNC_OPTION) . $(USER)@$(HOST):"~$(ROOTPATH)"
rsync-test:
	rsync $(_RSYNC_OPTION) $(RSYNC_OPTION) -n . $(USER)@$(HOST):"~$(ROOTPATH)"
