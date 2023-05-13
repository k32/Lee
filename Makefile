DOCBOOK := _build/lee_doc/test_out.xml
WWW := _build/lee_doc/html/index.html

MANPAGE_STYLESHEET ?= /usr/share/xml/docbook/?chap/docbook-xsl/manpages/docbook.xsl
XSLTNG := _build/lee_doc/docbook-xslTNG-2.1.2/libs/docbook-xslTNG-2.1.2.jar
all: compile linelimit test docbook_validate docs

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

.PHONY: docs _build/lee_doc/man/lee.1
docs: $(WWW) $(MANPAGE)

$(MANPAGE): $(DOCBOOK)
	xsltproc -o "$$(dirname $<)/../man/" $(MANPAGE_STYLESHEET) "$<"

$(DOCBOOK):
	rebar3 eunit

$(WWW): $(DOCBOOK) $(XSLTNG)
	mkdir -p "$$(dirname $@)"
	cd $$(dirname $@) ;\
	java -jar $(CURDIR)/$(XSLTNG) resource-base-uri='./' chunk-output-base-uri='./' \
                                verbatim-syntax-highlight-languages='bash erlang' \
                                chunk=index.html persistent-toc=true chunk-nav=true $(CURDIR)/$<
	cp -R _build/lee_doc/docbook-xslTNG-2.1.2/resources/* $$(dirname $@)

$(XSLTNG):
	cd _build/lee_doc/ && \
	wget https://github.com/docbook/xslTNG/releases/download/2.1.2/docbook-xslTNG-2.1.2.zip && \
	unzip docbook-xslTNG-2.1.2.zip
