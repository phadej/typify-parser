/* global describe:true, it:true */
"use strict";

var parser = require("../lib/parser.js");
var expect = require("chai").expect;
var fs = require("fs");
var path = require("path");

describe("fixtures", function () {
  var dirname = path.join(__dirname, "fixtures");
  var files = fs.readdirSync(dirname);
  files.forEach(function (file) {
    it(file, function () {
      var contents = fs.readFileSync(path.join(dirname, file)).toString();
      var lines = contents.split("\n");
      expect(lines.length).to.equal(3);
      expect(lines[2]).to.equal("");

      var signature = lines[0];
      var json = JSON.parse(lines[1]);

      expect(parser(signature)).to.deep.equal(json);
    });
  });
});
