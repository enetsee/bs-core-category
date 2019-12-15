// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Cartesian$CoreCategory = require("./Cartesian.bs.js");
var Cocartesian$CoreCategory = require("./Cocartesian.bs.js");

function Make2(X) {
  var include = Cartesian$CoreCategory.Make2({
        id: X.id,
        compose: X.compose,
        product: X.product,
        exl: X.exl,
        exr: X.exr
      });
  var $$let = Cocartesian$CoreCategory.Make2({
        id: X.id,
        compose: X.compose,
        inl: X.inl,
        inr: X.inr,
        sum: X.sum
      });
  return {
          distl: X.distl,
          distr: X.distr,
          CategoryInfix: include.CategoryInfix,
          product: X.product,
          exl: X.exl,
          exr: X.exr,
          CartesianInfix: include.CartesianInfix,
          $pipe$star$pipe: include.$pipe$star$pipe,
          id: X.id,
          compose: X.compose,
          $less$less: $$let.$less$less,
          $great$great: $$let.$great$great,
          inl: X.inl,
          inr: X.inr,
          sum: X.sum,
          CocartesianInfix: $$let.CocartesianInfix,
          $pipe$plus$pipe: $$let.$pipe$plus$pipe
        };
}

exports.Make2 = Make2;
/* No side effect */
