/* global describe:true, it:true */
"use strict";

var parser = require("../lib/parser.js");
var assert = require("assert");

describe("fixtures", function () {
  it("implement me", function () {
    assert(parser("foobar").length === 0);
  });
});
