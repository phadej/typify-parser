# typify type parser

> Type signature parser for typify

[![Build Status](https://secure.travis-ci.org/phadej/typify-parser.svg?branch=master)](http://travis-ci.org/phadej/typify-parser)
[![NPM version](https://badge.fury.io/js/typify-parser.svg)](http://badge.fury.io/js/typify-parser)
[![Dependency Status](https://david-dm.org/phadej/typify-parser.svg)](https://david-dm.org/phadej/typify-parser)
[![devDependency Status](https://david-dm.org/phadej/typify-parser/dev-status.svg)](https://david-dm.org/phadej/typify-parser#info=devDependencies)
[![Code Climate](https://img.shields.io/codeclimate/github/phadej/typify-parser.svg)](https://codeclimate.com/github/phadej/typify-parser)

Turns `(foo, bar 42) -> quux` into
```js
{
  "type": "fn",
  "var": {
    "args": [
      {
        "value": "foo",
        "type": "ident"
      },
      {
        "rator": {
          "value": "bar",
          "type": "ident"
        },
        "rands": [
          {
            "value": 42,
            "type": "number"
          }
        ],
        "type": "app"
      }
    ],
    "type": "prod"
  },
  "result": {
    "value": "quux",
    "type": "ident"
  }
}
```
