all: compile linelimit doc

.PHONY: compile
compile:
	rebar3 do dialyzer,eunit,ct,edoc,cover

.PHONY: linelimit
linelimit:
	support/linelimit

.PHONY: doc
doc: _build/top.texi _build/top_html/index.html

_build/top.texi: compile
	texi2any --info -o $@ test/top.texi

_build/top_html/index.html: compile
	texi2any --html -o $(dir $@) test/top.texi
