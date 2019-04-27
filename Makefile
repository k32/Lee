all: linelimit
	rebar3 do dialyzer,eunit,edoc

demo:
	rebar3 as demo escriptize

.PHONY: linelimit
linelimit:
	support/linelimit
