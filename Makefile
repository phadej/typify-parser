.PHONY : all test jshint jscs mocha istanbul dist literate

BINDIR=node_modules/.bin

JSHINT=$(BINDIR)/jshint
JSCS=$(BINDIR)/jscs
MOCHA=$(BINDIR)/_mocha
ISTANBUL=$(BINDIR)/istanbul
KARMA=$(BINDIR)/karma
BROWSERIFY=$(BINDIR)/browserify
LJS=$(BINDIR)/ljs

all : jshint jscs

test : jshint jscs mocha istanbul

jshint :
	$(JSHINT) lib test

jscs :
	$(JSCS) lib test

mocha :
	$(MOCHA) --reporter spec test

istanbul :
	$(ISTANBUL) cover $(MOCHA) test
	$(ISTANBUL) check-coverage --statements 100 --branches 100 --functions 100 coverage/coverage.json

literate :
	$(LJS) -c false -o README.md lib/parser.js

dist : test literate
	git clean -fdx -e node_modules
