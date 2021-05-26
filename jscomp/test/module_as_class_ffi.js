'use strict';

var Foo_class = require("xx/foo_class").default;
var Foo_class$1 = require("xx/foo_class");

function f(param) {
  return new Foo_class$1(3);
}

function v(param) {
  Foo_class$1.ff(3);
  return Foo_class(3);
}

exports.f = f;
exports.v = v;
/* xx/foo_class Not a pure module */
