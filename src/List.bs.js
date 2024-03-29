// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Monoid$CoreCategory = require("./Monoid.bs.js");
var Foldable$CoreCategory = require("./Foldable.bs.js");
var MonadPlus$CoreCategory = require("./MonadPlus.bs.js");

function pure(x) {
  return /* :: */[
          x,
          /* [] */0
        ];
}

function bind(x, f) {
  return Belt_List.flatten(Belt_List.map(x, f));
}

var map = Belt_List.map;

var include = MonadPlus$CoreCategory.Make1({
      pure: pure,
      bind: bind,
      map: map,
      alt: Pervasives.$at,
      empty: /* [] */0
    });

function foldLeft(t, f, init) {
  return Belt_List.reduce(t, init, f);
}

function foldRight_(t, f, init) {
  return Belt_List.reduceReverse(t, init, (function (y, x) {
                return Curry._2(f, x, y);
              }));
}

var foldRight = /* `Custom */[
  -198771759,
  foldRight_
];

var include$1 = Foldable$CoreCategory.MakeCustom1({
      foldLeft: foldLeft,
      foldRight: foldRight,
      foldMap: /* Derived */-684824643
    });

var include$2 = Monoid$CoreCategory.Make1({
      append: Pervasives.$at,
      mempty: /* [] */0
    });

var FunctorInfix = include.FunctorInfix;

var ApplyInfix = include.ApplyInfix;

var select = include.select;

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

var alt = include.alt;

var AltInfix = include.AltInfix;

var empty = include.empty;

var map$1 = include.map;

var replace = include.replace;

var $$void = include.$$void;

var $less$$great = include.$less$$great;

var $less$amp$great = include.$less$amp$great;

var $less$ = include.$less$;

var $$great = include.$$great;

var apply = include.apply;

var liftA2 = include.liftA2;

var applyFirst = include.applyFirst;

var applySecond = include.applySecond;

var $less$star$great = include.$less$star$great;

var $star$great = include.$star$great;

var $less$star = include.$less$star;

var $star$star = include.$star$star;

var liftA3 = include.liftA3;

var liftA4 = include.liftA4;

var liftA5 = include.liftA5;

var merge = include.merge;

var pure$1 = include.pure;

var when_ = include.when_;

var unless = include.unless;

var AlternativeInfix = include.AlternativeInfix;

var $less$pipe$great = include.$less$pipe$great;

var $less$slash$great = include.$less$slash$great;

var some = include.some;

var many = include.many;

var optional = include.optional;

var foldLeft$1 = include$1.foldLeft;

var foldRight$1 = include$1.foldRight;

var foldMap = include$1.foldMap;

var fold = include$1.fold;

var find = include$1.find;

var isEmpty = include$1.isEmpty;

var exists = include$1.exists;

var forAll = include$1.forAll;

var mempty = include$2.mempty;

var append = include$2.append;

var SemigroupInfix = include$2.SemigroupInfix;

var $less$great = include$2.$less$great;

exports.FunctorInfix = FunctorInfix;
exports.ApplyInfix = ApplyInfix;
exports.select = select;
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
exports.alt = alt;
exports.AltInfix = AltInfix;
exports.empty = empty;
exports.map = map$1;
exports.replace = replace;
exports.$$void = $$void;
exports.$less$$great = $less$$great;
exports.$less$amp$great = $less$amp$great;
exports.$less$ = $less$;
exports.$$great = $$great;
exports.apply = apply;
exports.liftA2 = liftA2;
exports.applyFirst = applyFirst;
exports.applySecond = applySecond;
exports.$less$star$great = $less$star$great;
exports.$star$great = $star$great;
exports.$less$star = $less$star;
exports.$star$star = $star$star;
exports.liftA3 = liftA3;
exports.liftA4 = liftA4;
exports.liftA5 = liftA5;
exports.merge = merge;
exports.pure = pure$1;
exports.when_ = when_;
exports.unless = unless;
exports.AlternativeInfix = AlternativeInfix;
exports.$less$pipe$great = $less$pipe$great;
exports.$less$slash$great = $less$slash$great;
exports.some = some;
exports.many = many;
exports.optional = optional;
exports.foldLeft = foldLeft$1;
exports.foldRight = foldRight$1;
exports.foldMap = foldMap;
exports.fold = fold;
exports.find = find;
exports.isEmpty = isEmpty;
exports.exists = exists;
exports.forAll = forAll;
exports.mempty = mempty;
exports.append = append;
exports.SemigroupInfix = SemigroupInfix;
exports.$less$great = $less$great;
/* include Not a pure module */
