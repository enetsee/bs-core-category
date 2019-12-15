// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Monad$CoreCategory = require("./Monad.bs.js");
var Foldable$CoreCategory = require("./Foldable.bs.js");
var Bifunctor$CoreCategory = require("./Bifunctor.bs.js");

function ok(x) {
  return /* Ok */Block.__(0, [x]);
}

function error(x) {
  return /* Error */Block.__(1, [x]);
}

function isError(param) {
  if (param.tag) {
    return true;
  } else {
    return false;
  }
}

function isOk(param) {
  if (param.tag) {
    return false;
  } else {
    return true;
  }
}

function result(t, withErr, withOk) {
  if (t.tag) {
    return Curry._1(withErr, t[0]);
  } else {
    return Curry._1(withOk, t[0]);
  }
}

function map(x, f) {
  if (x.tag) {
    return /* Error */Block.__(1, [x[0]]);
  } else {
    return /* Ok */Block.__(0, [Curry._1(f, x[0])]);
  }
}

function mapError(x, f) {
  if (x.tag) {
    return /* Error */Block.__(1, [Curry._1(f, x[0])]);
  } else {
    return /* Ok */Block.__(0, [x[0]]);
  }
}

function apply_001(x, f) {
  if (f.tag) {
    return /* Error */Block.__(1, [f[0]]);
  } else if (x.tag) {
    return /* Error */Block.__(1, [x[0]]);
  } else {
    return /* Ok */Block.__(0, [Curry._1(f[0], x[0])]);
  }
}

var apply = /* `Custom */[
  -198771759,
  apply_001
];

function select_001(x, f) {
  if (x.tag) {
    return /* Error */Block.__(1, [x[0]]);
  } else {
    var match = x[0];
    if (match.tag) {
      return /* Ok */Block.__(0, [match[0]]);
    } else {
      var a = match[0];
      return map(f, (function (g) {
                    return Curry._1(g, a);
                  }));
    }
  }
}

var select = /* `Custom */[
  -198771759,
  select_001
];

var map$1 = /* `Custom */[
  -198771759,
  map
];

function bind(x, f) {
  if (x.tag) {
    return /* Error */Block.__(1, [x[0]]);
  } else {
    return Curry._1(f, x[0]);
  }
}

var include = Monad$CoreCategory.MakeCustom2({
      pure: ok,
      bind: bind,
      map: map$1,
      replace: /* Derived */-684824643,
      apply: apply,
      liftA2: /* Derived */-684824643,
      applyFirst: /* Derived */-684824643,
      applySecond: /* Derived */-684824643,
      select: select
    });

var map$2 = include.map;

function bimap(x, first, second) {
  if (x.tag) {
    return /* Error */Block.__(1, [Curry._1(second, x[0])]);
  } else {
    return /* Ok */Block.__(0, [Curry._1(first, x[0])]);
  }
}

var mapFirst = /* `Custom */[
  -198771759,
  map$2
];

var mapSecond = /* `Custom */[
  -198771759,
  mapError
];

var include$1 = Bifunctor$CoreCategory.MakeCustom2({
      bimap: bimap,
      mapFirst: mapFirst,
      mapSecond: mapSecond
    });

function foldLeft(t, f, init) {
  if (t.tag) {
    return init;
  } else {
    return Curry._2(f, init, t[0]);
  }
}

var include$2 = Foldable$CoreCategory.Make2({
      foldLeft: foldLeft
    });

var replace = include.replace;

var $$void = include.$$void;

var FunctorInfix = include.FunctorInfix;

var $less$$great = include.$less$$great;

var $less$amp$great = include.$less$amp$great;

var $less$ = include.$less$;

var $$great = include.$$great;

var apply$1 = include.apply;

var applyFirst = include.applyFirst;

var applySecond = include.applySecond;

var liftA2 = include.liftA2;

var ApplyInfix = include.ApplyInfix;

var $less$star$great = include.$less$star$great;

var $star$great = include.$star$great;

var $less$star = include.$less$star;

var $star$star = include.$star$star;

var liftA3 = include.liftA3;

var liftA4 = include.liftA4;

var liftA5 = include.liftA5;

var merge = include.merge;

var pure = include.pure;

var when_ = include.when_;

var unless = include.unless;

var select$1 = include.select;

var SelectiveInfix = include.SelectiveInfix;

var $less$star$question = include.$less$star$question;

var $less$pipe$pipe$great = include.$less$pipe$pipe$great;

var $less$amp$amp$great = include.$less$amp$amp$great;

var orS = include.orS;

var andS = include.andS;

var whenS = include.whenS;

var branch = include.branch;

var ifS = include.ifS;

var fromOptionS = include.fromOptionS;

var anyS = include.anyS;

var allS = include.allS;

var whileS = include.whileS;

var bind$1 = include.bind;

var MonadInfix = include.MonadInfix;

var $great$great$eq = include.$great$great$eq;

var $great$great$tilde = include.$great$great$tilde;

var $great$eq$great = include.$great$eq$great;

var join = include.join;

var forever = include.forever;

var sequenceM = include.sequenceM;

var mapM = include.mapM;

var mapM_ = include.mapM_;

var bimap$1 = include$1.bimap;

var mapFirst$1 = include$1.mapFirst;

var mapSecond$1 = include$1.mapSecond;

var foldLeft$1 = include$2.foldLeft;

var foldRight = include$2.foldRight;

var foldMap = include$2.foldMap;

var fold = include$2.fold;

var find = include$2.find;

var isEmpty = include$2.isEmpty;

var exists = include$2.exists;

var forAll = include$2.forAll;

exports.ok = ok;
exports.error = error;
exports.isError = isError;
exports.isOk = isOk;
exports.result = result;
exports.mapError = mapError;
exports.map = map$2;
exports.replace = replace;
exports.$$void = $$void;
exports.FunctorInfix = FunctorInfix;
exports.$less$$great = $less$$great;
exports.$less$amp$great = $less$amp$great;
exports.$less$ = $less$;
exports.$$great = $$great;
exports.apply = apply$1;
exports.applyFirst = applyFirst;
exports.applySecond = applySecond;
exports.liftA2 = liftA2;
exports.ApplyInfix = ApplyInfix;
exports.$less$star$great = $less$star$great;
exports.$star$great = $star$great;
exports.$less$star = $less$star;
exports.$star$star = $star$star;
exports.liftA3 = liftA3;
exports.liftA4 = liftA4;
exports.liftA5 = liftA5;
exports.merge = merge;
exports.pure = pure;
exports.when_ = when_;
exports.unless = unless;
exports.select = select$1;
exports.SelectiveInfix = SelectiveInfix;
exports.$less$star$question = $less$star$question;
exports.$less$pipe$pipe$great = $less$pipe$pipe$great;
exports.$less$amp$amp$great = $less$amp$amp$great;
exports.orS = orS;
exports.andS = andS;
exports.whenS = whenS;
exports.branch = branch;
exports.ifS = ifS;
exports.fromOptionS = fromOptionS;
exports.anyS = anyS;
exports.allS = allS;
exports.whileS = whileS;
exports.bind = bind$1;
exports.MonadInfix = MonadInfix;
exports.$great$great$eq = $great$great$eq;
exports.$great$great$tilde = $great$great$tilde;
exports.$great$eq$great = $great$eq$great;
exports.join = join;
exports.forever = forever;
exports.sequenceM = sequenceM;
exports.mapM = mapM;
exports.mapM_ = mapM_;
exports.bimap = bimap$1;
exports.mapFirst = mapFirst$1;
exports.mapSecond = mapSecond$1;
exports.foldLeft = foldLeft$1;
exports.foldRight = foldRight;
exports.foldMap = foldMap;
exports.fold = fold;
exports.find = find;
exports.isEmpty = isEmpty;
exports.exists = exists;
exports.forAll = forAll;
/* include Not a pure module */
