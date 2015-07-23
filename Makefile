.PHONY : all test jshint eslint jscs mocha istanbul dist literate

BINDIR=node_modules/.bin

JSHINT=$(BINDIR)/jshint
ESLINT=$(BINDIR)/eslint
JSCS=$(BINDIR)/jscs
MOCHA=$(BINDIR)/mocha
IMOCHA=$(BINDIR)/_mocha
ISTANBUL=$(BINDIR)/istanbul
KARMA=$(BINDIR)/karma
BROWSERIFY=$(BINDIR)/browserify
LJS=$(BINDIR)/ljs
DAVID=$(BINDIR)/david

all : jshint jscs eslint

test : jshint jscs eslint mocha istanbul david

jshint :
	$(JSHINT) lib test

eslint :
	$(ESLINT) lib test

jscs :
	$(JSCS) lib test

mocha :
	$(MOCHA) --reporter spec test

istanbul :
	$(ISTANBUL) cover -- $(IMOCHA) test
	test -f coverage/coverage.json
	$(ISTANBUL) check-coverage --statements 100 --branches 100 --functions 100 coverage/coverage.json

literate :
	$(LJS) -c false -o README.md lib/parser.js

david :
	$(DAVID)

dist : test literate
	git clean -fdx -e node_modules
