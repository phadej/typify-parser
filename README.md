# typify type parser

> Type signature parser for typify

Turns `foo -> bar 42 -> quux` into
```js
{
  "type": "function",
  "arguments": [
    {
      "type": "name",
      "name": "foo"
    },
    {
      "type": "call",
      "function": {
        "type": "name",
        "name": "bar"
      },
      "arguments": [
        {
          "type": "number",
          "value": 42
        }
      ]
    }
  ],
  "result": {
    "type": "name",
    "name": "quux"
  }
}
```
