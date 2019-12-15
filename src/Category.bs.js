// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");

function Make2(X) {
  var compose = X.compose;
  var $less$less = Curry.__2(compose);
  var $great$great = function (g, f) {
    return Curry._2(compose, f, g);
  };
  var CategoryInfix = {
    $less$less: $less$less,
    $great$great: $great$great
  };
  return {
          id: X.id,
          compose: compose,
          CategoryInfix: CategoryInfix,
          $less$less: $less$less,
          $great$great: $great$great
        };
}

exports.Make2 = Make2;
/* No side effect */
