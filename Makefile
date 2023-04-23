all: compile linelimit test docbook_validate

.PHONY: compile
compile:
	rebar3 do dialyzer,eunit,ct,edoc,cover

.PHONY: demo
demo:
	rebar3 as demo escriptize

.PHONY: linelimit
linelimit:
	support/linelimit

.PHONY: docbook_validate
docbook_validate: _build/lee_doc/xsd/xlink.xsd _build/lee_doc/xsd/docbook.xsd compile
	xmllint --noout --schema _build/lee_doc/xsd/docbook.xsd '_build/lee_doc/test_out.xml'

# Fetch DocBook XSD schema for testing
_build/lee_doc/xsd/docbook.xsd: _build/lee_doc/xsd
	curl -o $@ https://docbook.org/xml/5.0/xsd/docbook.xsd

_build/lee_doc/xsd/xlink.xsd: _build/lee_doc/xsd
	curl -o $@ https://www.w3.org/1999/xlink.xsd

_build/lee_doc/xsd:
	mkdir -p $@
