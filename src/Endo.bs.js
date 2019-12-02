// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Fun$CoreCategory = require("./Fun.bs.js");
var Monoid$CoreCategory = require("./Monoid.bs.js");

function Make(X) {
  var combine = function (f, g) {
    return (function (param) {
        return Fun$CoreCategory.compose(f, g, param);
      });
  };
  var include = Monoid$CoreCategory.Make0({
        combine: combine,
        empty: Fun$CoreCategory.id
      });
  var runEndo = function (t, x) {
    return Curry._1(t, x);
  };
  return {
          empty: include.empty,
          combine: include.combine,
          Semigroup_infix: include.Semigroup_infix,
          $less$great: include.$less$great,
          runEndo: runEndo
        };
}

exports.Make = Make;
/* Fun-CoreCategory Not a pure module */
