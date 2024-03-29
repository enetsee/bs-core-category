// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Bifunctor$CoreCategory = require("./Bifunctor.bs.js");

function first(a) {
  return /* First */Block.__(0, [a]);
}

function second(a) {
  return /* Second */Block.__(1, [a]);
}

function either(first, second, param) {
  if (param.tag) {
    return Curry._1(second, param[0]);
  } else {
    return Curry._1(first, param[0]);
  }
}

function isFirst(param) {
  if (param.tag) {
    return false;
  } else {
    return true;
  }
}

function isSecond(param) {
  if (param.tag) {
    return true;
  } else {
    return false;
  }
}

function fromFirst($$default, param) {
  if (param.tag) {
    return $$default;
  } else {
    return param[0];
  }
}

function fromSecond($$default, param) {
  if (param.tag) {
    return param[0];
  } else {
    return $$default;
  }
}

function bimap(t, first, second) {
  if (t.tag) {
    return /* Second */Block.__(1, [Curry._1(second, t[0])]);
  } else {
    return /* First */Block.__(0, [Curry._1(first, t[0])]);
  }
}

var include = Bifunctor$CoreCategory.Make2({
      bimap: bimap
    });

var bimap$1 = include.bimap;

var mapFirst = include.mapFirst;

var mapSecond = include.mapSecond;

exports.first = first;
exports.second = second;
exports.either = either;
exports.isFirst = isFirst;
exports.isSecond = isSecond;
exports.fromFirst = fromFirst;
exports.fromSecond = fromSecond;
exports.bimap = bimap$1;
exports.mapFirst = mapFirst;
exports.mapSecond = mapSecond;
/* include Not a pure module */
