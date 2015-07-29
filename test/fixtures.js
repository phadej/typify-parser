/* global describe:true, it:true */
"use strict";

var parser = require("../lib/parser.js");
var expect = require("chai").expect;
var fs = require("fs");
var path = require("path");

var BUCKET_SIZE = parseInt(process.env.FIXTUREBUCKET, 10) || 10;

function bucket(array, size) {
  var len = array.length;
  var res = [];
  for (var i = 0; i < len; i += size) {
    res.push(array.slice(i, i + size));
  }
  return res;
}

describe("fixtures", function () {
  var dirname = path.join(__dirname, "fixtures");
  var files = fs.readdirSync(dirname);
  files.sort();
  var fileBuckets = bucket(files, BUCKET_SIZE);

  fileBuckets.forEach(function (b) {
    it(b[0] + " -- " + b[b.length - 1], function () {
      b.forEach(function (file) {
        var contents = fs.readFileSync(path.join(dirname, file)).toString();
        var lines = contents.split("\n");
        expect(lines.length).to.equal(4);
        expect(lines[3]).to.equal("");

        var signature = lines[0];
        var fvs = JSON.parse(lines[1]);
        var json = JSON.parse(lines[2]);

        // temporary
        expect(fvs.every(function (el) { return typeof el === "string"; })).to.equal(true);
        expect(parser(signature)).to.deep.equal(json);
      });
    });
  });
});
