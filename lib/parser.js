/**
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
*/
"use strict";

function unescapeString(str) {
  return str.replace(/\\(?:'|"|\\|n|x[0-9a-fA-F]{2})/g, function (match) {
    switch (match[1]) {
      case "'": return "'";
      case "\"": return "\"";
      case "\\": return "\\";
      case "n": return "\n";
      case "x": return String.fromCharCode(parseInt(match.substr(2), 16));
    }
  });
}

function lex(input) {
  // TODO: unicode
  var m = input.match(/^([ \t\r\n]+|_\|_|\*|\(\)|"(?:[^"\\]|\\[\\'"n]|\\x[0-9a-fA-F]{2})*"|'(?:[^'\\]|\\[\\'"n]|\\x[0-9a-fA-F]{2})*'|[0-9a-zA-Z_\$@]+|,|->|:|;|&|\||\.\.\.|\(|\)|\[|\]|\{|\}|\?)*$/);
  if (m === null) {
    throw new SyntaxError("Cannot lex type signature");
  }
  m = input.match(/([ \t\r\n]+|_\|_|\*|\(\)|"(?:[^"\\]|\\[\\'"n]|\\x[0-9a-fA-F]{2})*"|'(?:[^'\\]|\\[\\'"n]|\\x[0-9a-fA-F]{2})*'|[0-9a-zA-Z_\$@]+|,|->|:|;|&|\||\.\.\.|\(|\)|\[|\]|\{|\}|\?)/g);

  return m
  .map(function (token) {
    switch (token) {
      case "_|_": return { type: "false" };
      case "*": return { type: "true" };
      case "()": return { type: "unit" };
      case "true": return { type: "bool", value: true };
      case "false": return { type: "bool", value: false };
      case "&": return { type: "&" };
      case "|": return { type: "|" };
      case ",": return { type: "," };
      case ";": return { type: ";" };
      case ":": return { type: ":" };
      case "(": return { type: "(" };
      case ")": return { type: ")" };
      case "[": return { type: "[" };
      case "]": return { type: "]" };
      case "{": return { type: "{" };
      case "}": return { type: "}" };
      case "?": return { type: "?" };
      case "->": return { type: "->" };
      case "...": return { type: "..." };
    }

    // Whitespace
    if (token.match(/^[ \r\r\n]+$/)) {
      return null;
    }

    if (token.match(/^[0-9]+/)) {
      return { type: "number", value: parseInt(token, 10) };
    }

    if (token[0] === "'" || token[0] === "\"") {
      token = token.slice(1, -1);
      return { type: "string", value: unescapeString(token) };
    }

    return { type: "ident", value: token };
  })
  .filter(function (token) {
    return token !== null;
  });
}

function makePunctParser(type) {
  return function (state) {
    if (state.pos >= state.len) {
      throw new SyntaxError("Expecting identifier, end-of-input found");
    }

    var token = state.tokens[state.pos];
    if (token.type !== type) {
      throw new SyntaxError("Expecting '" + type + "', found: " + token.type);
    }
    state.pos += 1;

    return type;
  };
}

var colonParser = makePunctParser(":");
var openCurlyParser = makePunctParser("{");
var closeCurlyParser = makePunctParser("}");
var semicolonParser = makePunctParser(";");
var openParenParser = makePunctParser("(");
var closeParenParser = makePunctParser(")");
var openBracketParser = makePunctParser("[");
var closeBracketParser = makePunctParser("]");

function nameParser(state) {
  if (state.pos >= state.len) {
    throw new SyntaxError("Expecting identifier, end-of-input found");
  }

  var token = state.tokens[state.pos];
  if (token.type !== "ident") {
    throw new SyntaxError("Expecting 'ident', found: " + token.type);
  }
  state.pos += 1;

  return token.value;
}

function recordParser(state) {
  openCurlyParser(state);

  var token = state.tokens[state.pos];
  if (token && token.type === "}") {
    closeCurlyParser(state);
    return { type: "record", fields: {} };
  }

  var fields = {};

  while (true) {
    // read
    var name = nameParser(state);
    colonParser(state);
    var value = typeParser(state);

    // assign to fields
    fields[name] = value;

    // ending token
    token = state.tokens[state.pos];

    // break if }
    if (token && token.type === "}") {
      closeCurlyParser(state);
      break;
    } else if (token && token.type === ";") {
      semicolonParser(state);
    } else {
      throw new SyntaxError("Expecting '}' or ';', found: " + token.type);
    }
  }

  return { type: "record", fields: fields };
}

function optionalParser(state) {
  var arg = terminalParser(state);

  var token = state.tokens[state.pos];
  if (token && token.type === "?") {
    state.pos += 1;
    return {
      type: "optional",
      arg: arg,
    };
  } else {
    return arg;
  }
}

function applicationParser(state) {
  var rator = optionalParser(state);
  var rands = [];

  while (true) {
    var pos = state.pos;
    // XXX: we shouldn't use exceptions for this
    try {
      var arg = optionalParser(state);
      rands.push(arg);
    } catch (err) {
      state.pos = pos;
      break;
    }
  }

  if (rands.length === 0) {
    return rator;
  } else {
    return {
      type: "app",
      rator: rator,
      rands: rands,
    };
  }
}

function separatedBy(parser, separator, constructor) {
  return function (state) {
    var list = [parser(state)];
    while (true) {
      // separator
      var token = state.tokens[state.pos];
      if (token && token.type === separator) {
        state.pos += 1;
      } else {
        break;
      }

      // right argument
      list.push(parser(state));
    }

    if (list.length === 1) {
      return list[0];
    } else {
      return {
        type: constructor,
        args: list,
      };
    }
  };
}

var conjunctionParser = separatedBy(applicationParser, "&", "conj");
var disjunctionParser = separatedBy(conjunctionParser, "|", "disj");

// TODO: combine with optional
function variadicParser(state) {
  var arg = disjunctionParser(state);
  var token = state.tokens[state.pos];
  if (token && token.type === "...") {
    state.pos += 1;
    return {
      type: "variadic",
      arg: arg,
    };
  } else {
    return arg;
  }  
}

function namedParser(state) {
  var token1 = state.tokens[state.pos];
  var token2 = state.tokens[state.pos + 1];
  if (token1 && token2 && token1.type === "ident" && token2.type === ":") {
    state.pos += 2;
    var arg = namedParser(state);
    return {
      type: "named",
      name: token1.value,
      arg: arg,
    };
  } else {  
    return variadicParser(state);
  }
}

var productParser = separatedBy(namedParser, ",", "prod");

function functionParser(state) {
  var v = productParser(state);

  var token = state.tokens[state.pos];
  if (token && token.type === "->") {
    state.pos += 1;
    var result = functionParser(state);
    return {
      type: "fn",
      "var": v,
      result: result,
    };
  } else {
    return v;
  }
}

function typeParser(state) {
  return functionParser(state);
}

function parenthesesParser(state) {
  openParenParser(state);
  var type = typeParser(state);
  closeParenParser(state);
  return type;
}

function bracketParser(state) {
  openBracketParser(state);
  var type = typeParser(state);
  closeBracketParser(state);
  return {
    type: "brackets",
    arg: type,
  };
}

function terminalParser(state) {
  if (state.pos >= state.len) {
    throw new SyntaxError("Expecting terminal, end-of-input found");
  }

  var token = state.tokens[state.pos];
  switch (token.type) {
    case "false":
    case "true":
    case "unit":
    case "string":
    case "number":
    case "bool":
    case "ident":
      state.pos += 1;
      return token;
    case "{":
      return recordParser(state);
    case "(":
      return parenthesesParser(state);
    case "[":
      return bracketParser(state);
    default:
      throw new SyntaxError("Expecting terminal, " + token.type + " found");
  }
}

function parse(input) {
  // console.log(input);
  var tokens = lex(input);
  // console.log(tokens);
  var state = {
    pos: 0,
    len: tokens.length,
    tokens: tokens,
  };

  var res = typeParser(state);
  // console.log(state);
  if (state.pos !== state.len) {
    throw new SyntaxError("expecting end-of-input, " + tokens[state.pos].type + " found");
  }
  return res;
}

module.exports = parse;
