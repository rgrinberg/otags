

otags.man: otags.1
	nroff -man $< > $@

otags.html: otags.1
	man2html $< | \
		tail -n +3 |\
		sed -e 's|<A HREF=".*Return to Main Contents</A>||' \
		    -e 's|/cgi-bin/man/man2html?1.\([^"]*\)|http://www.linuxmanpages.com/man1/\1.1.php|' \
		    -e 's|HREF="/cgi-bin/man/man2html">man2html|HREF="http://www.nongnu.org/man2html/">man2html|' \
		> $@


.PHONY: clean
clean:
	rm -f otags.man
