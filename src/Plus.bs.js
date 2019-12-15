// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Alt$CoreCategory = require("./Alt.bs.js");

function S1_to_S2(X) {
  return X;
}

function S2_to_S1(X) {
  return X;
}

function S2_to_S3(X) {
  return X;
}

function S3_to_S2(X) {
  return X;
}

function MakeCustom3(X) {
  var include = Alt$CoreCategory.MakeCustom3(X);
  return {
          map: include.map,
          replace: include.replace,
          $$void: include.$$void,
          FunctorInfix: include.FunctorInfix,
          $less$$great: include.$less$$great,
          $less$amp$great: include.$less$amp$great,
          $less$: include.$less$,
          $$great: include.$$great,
          alt: include.alt,
          AltInfix: include.AltInfix,
          $less$pipe$great: include.$less$pipe$great,
          empty: X.empty
        };
}

function MakeCustom2(X) {
  var empty = X.empty;
  var X_map = X.map;
  var X_replace = X.replace;
  var X_alt = X.alt;
  var X$1 = {
    map: X_map,
    replace: X_replace,
    alt: X_alt,
    empty: empty
  };
  var include = Alt$CoreCategory.MakeCustom3(X$1);
  return {
          map: include.map,
          replace: include.replace,
          $$void: include.$$void,
          FunctorInfix: include.FunctorInfix,
          $less$$great: include.$less$$great,
          $less$amp$great: include.$less$amp$great,
          $less$: include.$less$,
          $$great: include.$$great,
          alt: include.alt,
          AltInfix: include.AltInfix,
          $less$pipe$great: include.$less$pipe$great,
          empty: empty
        };
}

function MakeCustom1(X) {
  var empty = X.empty;
  var X_map = X.map;
  var X_replace = X.replace;
  var X_alt = X.alt;
  var X$1 = {
    map: X_map,
    replace: X_replace,
    alt: X_alt,
    empty: empty
  };
  var include = Alt$CoreCategory.MakeCustom3(X$1);
  return {
          map: include.map,
          replace: include.replace,
          $$void: include.$$void,
          FunctorInfix: include.FunctorInfix,
          $less$$great: include.$less$$great,
          $less$amp$great: include.$less$amp$great,
          $less$: include.$less$,
          $$great: include.$$great,
          alt: include.alt,
          AltInfix: include.AltInfix,
          $less$pipe$great: include.$less$pipe$great,
          empty: empty
        };
}

function Make3(X) {
  var empty = X.empty;
  var X_map = X.map;
  var X_alt = X.alt;
  var X$1 = {
    map: X_map,
    replace: /* Derived */-684824643,
    alt: X_alt,
    empty: empty
  };
  var include = Alt$CoreCategory.MakeCustom3(X$1);
  return {
          map: include.map,
          replace: include.replace,
          $$void: include.$$void,
          FunctorInfix: include.FunctorInfix,
          $less$$great: include.$less$$great,
          $less$amp$great: include.$less$amp$great,
          $less$: include.$less$,
          $$great: include.$$great,
          alt: include.alt,
          AltInfix: include.AltInfix,
          $less$pipe$great: include.$less$pipe$great,
          empty: empty
        };
}

function Make2(X) {
  var empty = X.empty;
  var X_map = X.map;
  var X_alt = X.alt;
  var X$1 = {
    map: X_map,
    replace: /* Derived */-684824643,
    alt: X_alt,
    empty: empty
  };
  var include = Alt$CoreCategory.MakeCustom3(X$1);
  return {
          map: include.map,
          replace: include.replace,
          $$void: include.$$void,
          FunctorInfix: include.FunctorInfix,
          $less$$great: include.$less$$great,
          $less$amp$great: include.$less$amp$great,
          $less$: include.$less$,
          $$great: include.$$great,
          alt: include.alt,
          AltInfix: include.AltInfix,
          $less$pipe$great: include.$less$pipe$great,
          empty: empty
        };
}

function Make1(X) {
  var empty = X.empty;
  var X_map = X.map;
  var X_alt = X.alt;
  var X$1 = {
    map: X_map,
    replace: /* Derived */-684824643,
    alt: X_alt,
    empty: empty
  };
  var include = Alt$CoreCategory.MakeCustom3(X$1);
  return {
          map: include.map,
          replace: include.replace,
          $$void: include.$$void,
          FunctorInfix: include.FunctorInfix,
          $less$$great: include.$less$$great,
          $less$amp$great: include.$less$amp$great,
          $less$: include.$less$,
          $$great: include.$$great,
          alt: include.alt,
          AltInfix: include.AltInfix,
          $less$pipe$great: include.$less$pipe$great,
          empty: empty
        };
}

exports.S1_to_S2 = S1_to_S2;
exports.S2_to_S1 = S2_to_S1;
exports.S2_to_S3 = S2_to_S3;
exports.S3_to_S2 = S3_to_S2;
exports.MakeCustom1 = MakeCustom1;
exports.MakeCustom2 = MakeCustom2;
exports.MakeCustom3 = MakeCustom3;
exports.Make1 = Make1;
exports.Make2 = Make2;
exports.Make3 = Make3;
/* No side effect */