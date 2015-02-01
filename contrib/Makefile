
install:
	install -m 755 update-otags $(DESTDIR)/usr/sbin
	install -m 644 update-otags.8 \
		$(DESTDIR)/usr/share/man/man8/update-otags.8

update-otags.man: update-otags.8
	nroff -man $< > $@

.PHONY: clean
clean:
	rm -f update-otags.man

