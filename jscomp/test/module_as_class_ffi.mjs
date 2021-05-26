

import Foo_class from "xx/foo_class";
import * as Foo_class$1 from "xx/foo_class";

function f(param) {
  return new Foo_class$1(3);
}

function v(param) {
  Foo_class$1.ff(3);
  return Foo_class(3);
}

export {
  f ,
  v ,
  
}
/* xx/foo_class Not a pure module */
