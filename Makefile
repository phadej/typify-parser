.PHONY : all test mocha istanbul dist

BINDIR=node_modules/.bin

JSHINT=$(BINDIR)/jshint
JSCS=$(BINDIR)/jscs
MOCHA=$(BINDIR)/_mocha
ISTANBUL=$(BINDIR)/istanbul
KARMA=$(BINDIR)/karma
BROWSERIFY=$(BINDIR)/browserify
LJS=$(BINDIR)/ljs

PSC=psc
PSCMAKE=psc-make

DISTDIR=dist

SRC=src/parser.purs
DIST=$(DISTDIR)/parser.js

NODE_ENV=$(DISTDIR)

all : $(DIST)

test : $(DIST) mocha istanbul

$(DIST) : $(SRC)
	find src bower_components -name '*.purs'|xargs $(PSC) -m Typify --codegen Typify -o $(DIST)
	find src bower_components -name '*.purs'|xargs $(PSCMAKE) -o dist

mocha : $(DIST)
	$(MOCHA) -b --reporter spec test

istanbul : $(DIST)
	$(ISTANBUL) cover $(MOCHA) test
	$(ISTANBUL) check-coverage --statements 100 --branches 100 --functions 100 coverage/coverage.json

dist : test
	git clean -fdx -e node_modules
