# typify type parser

> Type signature parser for typify

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
