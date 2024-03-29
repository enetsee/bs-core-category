// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Monad$CoreCategory = require("./Monad.bs.js");
var Alternative$CoreCategory = require("./Alternative.bs.js");

function S1_to_S2(X) {
  return {
          FunctorInfix: X.FunctorInfix,
          ApplyInfix: X.ApplyInfix,
          select: X.select,
          SelectiveInfix: X.SelectiveInfix,
          $less$star$question: X.$less$star$question,
          $less$pipe$pipe$great: X.$less$pipe$pipe$great,
          $less$amp$amp$great: X.$less$amp$amp$great,
          orS: X.orS,
          andS: X.andS,
          whenS: X.whenS,
          branch: X.branch,
          ifS: X.ifS,
          fromOptionS: X.fromOptionS,
          anyS: X.anyS,
          allS: X.allS,
          whileS: X.whileS,
          bind: X.bind,
          MonadInfix: X.MonadInfix,
          $great$great$eq: X.$great$great$eq,
          $great$great$tilde: X.$great$great$tilde,
          $great$eq$great: X.$great$eq$great,
          join: X.join,
          forever: X.forever,
          sequenceM: X.sequenceM,
          mapM: X.mapM,
          mapM_: X.mapM_,
          alt: X.alt,
          AltInfix: X.AltInfix,
          empty: X.empty,
          map: X.map,
          replace: X.replace,
          $$void: X.$$void,
          $less$$great: X.$less$$great,
          $less$amp$great: X.$less$amp$great,
          $less$: X.$less$,
          $$great: X.$$great,
          apply: X.apply,
          applyFirst: X.applyFirst,
          applySecond: X.applySecond,
          liftA2: X.liftA2,
          $less$star$great: X.$less$star$great,
          $star$great: X.$star$great,
          $less$star: X.$less$star,
          $star$star: X.$star$star,
          liftA3: X.liftA3,
          liftA4: X.liftA4,
          liftA5: X.liftA5,
          merge: X.merge,
          pure: X.pure,
          when_: X.when_,
          unless: X.unless,
          AlternativeInfix: X.AlternativeInfix,
          $less$pipe$great: X.$less$pipe$great,
          $less$slash$great: X.$less$slash$great,
          some: X.some,
          many: X.many,
          optional: X.optional
        };
}

function S2_to_S1(X) {
  return {
          FunctorInfix: X.FunctorInfix,
          ApplyInfix: X.ApplyInfix,
          select: X.select,
          SelectiveInfix: X.SelectiveInfix,
          $less$star$question: X.$less$star$question,
          $less$pipe$pipe$great: X.$less$pipe$pipe$great,
          $less$amp$amp$great: X.$less$amp$amp$great,
          orS: X.orS,
          andS: X.andS,
          whenS: X.whenS,
          branch: X.branch,
          ifS: X.ifS,
          fromOptionS: X.fromOptionS,
          anyS: X.anyS,
          allS: X.allS,
          whileS: X.whileS,
          bind: X.bind,
          MonadInfix: X.MonadInfix,
          $great$great$eq: X.$great$great$eq,
          $great$great$tilde: X.$great$great$tilde,
          $great$eq$great: X.$great$eq$great,
          join: X.join,
          forever: X.forever,
          sequenceM: X.sequenceM,
          mapM: X.mapM,
          mapM_: X.mapM_,
          alt: X.alt,
          AltInfix: X.AltInfix,
          empty: X.empty,
          map: X.map,
          replace: X.replace,
          $$void: X.$$void,
          $less$$great: X.$less$$great,
          $less$amp$great: X.$less$amp$great,
          $less$: X.$less$,
          $$great: X.$$great,
          apply: X.apply,
          liftA2: X.liftA2,
          applyFirst: X.applyFirst,
          applySecond: X.applySecond,
          $less$star$great: X.$less$star$great,
          $star$great: X.$star$great,
          $less$star: X.$less$star,
          $star$star: X.$star$star,
          liftA3: X.liftA3,
          liftA4: X.liftA4,
          liftA5: X.liftA5,
          merge: X.merge,
          pure: X.pure,
          when_: X.when_,
          unless: X.unless,
          AlternativeInfix: X.AlternativeInfix,
          $less$pipe$great: X.$less$pipe$great,
          $less$slash$great: X.$less$slash$great,
          some: X.some,
          many: X.many,
          optional: X.optional
        };
}

function S2_to_S3(X) {
  return X;
}

function S3_to_S2(X) {
  return X;
}

function MakeCustom3(X) {
  var map_001 = X.map;
  var map = /* `Custom */[
    -198771759,
    map_001
  ];
  var M = Monad$CoreCategory.MakeCustom3({
        pure: X.pure,
        bind: X.bind,
        map: map,
        replace: X.replace,
        apply: X.apply,
        liftA2: X.liftA2,
        applyFirst: X.applyFirst,
        applySecond: X.applySecond,
        select: X.select,
        join: X.join
      });
  var map_001$1 = M.map;
  var map$1 = /* `Custom */[
    -198771759,
    map_001$1
  ];
  var $$let = Alternative$CoreCategory.MakeCustom3({
        alt: X.alt,
        empty: X.empty,
        pure: X.pure,
        apply: M.apply,
        map: map$1,
        replace: X.replace,
        liftA2: X.liftA2,
        applyFirst: X.applyFirst,
        applySecond: X.applySecond
      });
  return {
          FunctorInfix: M.FunctorInfix,
          ApplyInfix: M.ApplyInfix,
          select: M.select,
          SelectiveInfix: M.SelectiveInfix,
          $less$star$question: M.$less$star$question,
          $less$pipe$pipe$great: M.$less$pipe$pipe$great,
          $less$amp$amp$great: M.$less$amp$amp$great,
          orS: M.orS,
          andS: M.andS,
          whenS: M.whenS,
          branch: M.branch,
          ifS: M.ifS,
          fromOptionS: M.fromOptionS,
          anyS: M.anyS,
          allS: M.allS,
          whileS: M.whileS,
          bind: M.bind,
          MonadInfix: M.MonadInfix,
          $great$great$eq: M.$great$great$eq,
          $great$great$tilde: M.$great$great$tilde,
          $great$eq$great: M.$great$eq$great,
          join: M.join,
          forever: M.forever,
          sequenceM: M.sequenceM,
          mapM: M.mapM,
          mapM_: M.mapM_,
          alt: $$let.alt,
          AltInfix: $$let.AltInfix,
          empty: $$let.empty,
          map: $$let.map,
          replace: $$let.replace,
          $$void: $$let.$$void,
          $less$$great: $$let.$less$$great,
          $less$amp$great: $$let.$less$amp$great,
          $less$: $$let.$less$,
          $$great: $$let.$$great,
          apply: $$let.apply,
          applyFirst: $$let.applyFirst,
          applySecond: $$let.applySecond,
          liftA2: $$let.liftA2,
          $less$star$great: $$let.$less$star$great,
          $star$great: $$let.$star$great,
          $less$star: $$let.$less$star,
          $star$star: $$let.$star$star,
          liftA3: $$let.liftA3,
          liftA4: $$let.liftA4,
          liftA5: $$let.liftA5,
          merge: $$let.merge,
          pure: $$let.pure,
          when_: $$let.when_,
          unless: $$let.unless,
          AlternativeInfix: $$let.AlternativeInfix,
          $less$pipe$great: $$let.$less$pipe$great,
          $less$slash$great: $$let.$less$slash$great,
          some: $$let.some,
          many: $$let.many,
          optional: $$let.optional
        };
}

function MakeCustom2(X) {
  var pure = X.pure;
  var liftA2 = X.liftA2;
  var applyFirst = X.applyFirst;
  var applySecond = X.applySecond;
  var replace = X.replace;
  var map_001 = X.map;
  var map = /* `Custom */[
    -198771759,
    map_001
  ];
  var M = Monad$CoreCategory.MakeCustom3({
        pure: pure,
        bind: X.bind,
        map: map,
        replace: replace,
        apply: X.apply,
        liftA2: liftA2,
        applyFirst: applyFirst,
        applySecond: applySecond,
        select: X.select,
        join: X.join
      });
  var map_001$1 = M.map;
  var map$1 = /* `Custom */[
    -198771759,
    map_001$1
  ];
  var $$let = Alternative$CoreCategory.MakeCustom3({
        alt: X.alt,
        empty: X.empty,
        pure: pure,
        apply: M.apply,
        map: map$1,
        replace: replace,
        liftA2: liftA2,
        applyFirst: applyFirst,
        applySecond: applySecond
      });
  return {
          FunctorInfix: M.FunctorInfix,
          ApplyInfix: M.ApplyInfix,
          select: M.select,
          SelectiveInfix: M.SelectiveInfix,
          $less$star$question: M.$less$star$question,
          $less$pipe$pipe$great: M.$less$pipe$pipe$great,
          $less$amp$amp$great: M.$less$amp$amp$great,
          orS: M.orS,
          andS: M.andS,
          whenS: M.whenS,
          branch: M.branch,
          ifS: M.ifS,
          fromOptionS: M.fromOptionS,
          anyS: M.anyS,
          allS: M.allS,
          whileS: M.whileS,
          bind: M.bind,
          MonadInfix: M.MonadInfix,
          $great$great$eq: M.$great$great$eq,
          $great$great$tilde: M.$great$great$tilde,
          $great$eq$great: M.$great$eq$great,
          join: M.join,
          forever: M.forever,
          sequenceM: M.sequenceM,
          mapM: M.mapM,
          mapM_: M.mapM_,
          alt: $$let.alt,
          AltInfix: $$let.AltInfix,
          empty: $$let.empty,
          map: $$let.map,
          replace: $$let.replace,
          $$void: $$let.$$void,
          $less$$great: $$let.$less$$great,
          $less$amp$great: $$let.$less$amp$great,
          $less$: $$let.$less$,
          $$great: $$let.$$great,
          apply: $$let.apply,
          applyFirst: $$let.applyFirst,
          applySecond: $$let.applySecond,
          liftA2: $$let.liftA2,
          $less$star$great: $$let.$less$star$great,
          $star$great: $$let.$star$great,
          $less$star: $$let.$less$star,
          $star$star: $$let.$star$star,
          liftA3: $$let.liftA3,
          liftA4: $$let.liftA4,
          liftA5: $$let.liftA5,
          merge: $$let.merge,
          pure: $$let.pure,
          when_: $$let.when_,
          unless: $$let.unless,
          AlternativeInfix: $$let.AlternativeInfix,
          $less$pipe$great: $$let.$less$pipe$great,
          $less$slash$great: $$let.$less$slash$great,
          some: $$let.some,
          many: $$let.many,
          optional: $$let.optional
        };
}

function MakeCustom1(X) {
  var pure = X.pure;
  var liftA2 = X.liftA2;
  var applyFirst = X.applyFirst;
  var applySecond = X.applySecond;
  var replace = X.replace;
  var map_001 = X.map;
  var map = /* `Custom */[
    -198771759,
    map_001
  ];
  var M = Monad$CoreCategory.MakeCustom3({
        pure: pure,
        bind: X.bind,
        map: map,
        replace: replace,
        apply: X.apply,
        liftA2: liftA2,
        applyFirst: applyFirst,
        applySecond: applySecond,
        select: X.select,
        join: X.join
      });
  var map_001$1 = M.map;
  var map$1 = /* `Custom */[
    -198771759,
    map_001$1
  ];
  var $$let = Alternative$CoreCategory.MakeCustom3({
        alt: X.alt,
        empty: X.empty,
        pure: pure,
        apply: M.apply,
        map: map$1,
        replace: replace,
        liftA2: liftA2,
        applyFirst: applyFirst,
        applySecond: applySecond
      });
  return {
          FunctorInfix: M.FunctorInfix,
          ApplyInfix: M.ApplyInfix,
          select: M.select,
          SelectiveInfix: M.SelectiveInfix,
          $less$star$question: M.$less$star$question,
          $less$pipe$pipe$great: M.$less$pipe$pipe$great,
          $less$amp$amp$great: M.$less$amp$amp$great,
          orS: M.orS,
          andS: M.andS,
          whenS: M.whenS,
          branch: M.branch,
          ifS: M.ifS,
          fromOptionS: M.fromOptionS,
          anyS: M.anyS,
          allS: M.allS,
          whileS: M.whileS,
          bind: M.bind,
          MonadInfix: M.MonadInfix,
          $great$great$eq: M.$great$great$eq,
          $great$great$tilde: M.$great$great$tilde,
          $great$eq$great: M.$great$eq$great,
          join: M.join,
          forever: M.forever,
          sequenceM: M.sequenceM,
          mapM: M.mapM,
          mapM_: M.mapM_,
          alt: $$let.alt,
          AltInfix: $$let.AltInfix,
          empty: $$let.empty,
          map: $$let.map,
          replace: $$let.replace,
          $$void: $$let.$$void,
          $less$$great: $$let.$less$$great,
          $less$amp$great: $$let.$less$amp$great,
          $less$: $$let.$less$,
          $$great: $$let.$$great,
          apply: $$let.apply,
          liftA2: $$let.liftA2,
          applyFirst: $$let.applyFirst,
          applySecond: $$let.applySecond,
          $less$star$great: $$let.$less$star$great,
          $star$great: $$let.$star$great,
          $less$star: $$let.$less$star,
          $star$star: $$let.$star$star,
          liftA3: $$let.liftA3,
          liftA4: $$let.liftA4,
          liftA5: $$let.liftA5,
          merge: $$let.merge,
          pure: $$let.pure,
          when_: $$let.when_,
          unless: $$let.unless,
          AlternativeInfix: $$let.AlternativeInfix,
          $less$pipe$great: $$let.$less$pipe$great,
          $less$slash$great: $$let.$less$slash$great,
          some: $$let.some,
          many: $$let.many,
          optional: $$let.optional
        };
}

function Make3(X) {
  var pure = X.pure;
  var map_001 = X.map;
  var map = /* `Custom */[
    -198771759,
    map_001
  ];
  var M = Monad$CoreCategory.MakeCustom3({
        pure: pure,
        bind: X.bind,
        map: map,
        replace: /* Derived */-684824643,
        apply: /* Derived */-684824643,
        liftA2: /* Derived */-684824643,
        applyFirst: /* Derived */-684824643,
        applySecond: /* Derived */-684824643,
        select: /* Derived */-684824643,
        join: /* Derived */-684824643
      });
  var map_001$1 = M.map;
  var map$1 = /* `Custom */[
    -198771759,
    map_001$1
  ];
  var $$let = Alternative$CoreCategory.MakeCustom3({
        alt: X.alt,
        empty: X.empty,
        pure: pure,
        apply: M.apply,
        map: map$1,
        replace: /* Derived */-684824643,
        liftA2: /* Derived */-684824643,
        applyFirst: /* Derived */-684824643,
        applySecond: /* Derived */-684824643
      });
  return {
          FunctorInfix: M.FunctorInfix,
          ApplyInfix: M.ApplyInfix,
          select: M.select,
          SelectiveInfix: M.SelectiveInfix,
          $less$star$question: M.$less$star$question,
          $less$pipe$pipe$great: M.$less$pipe$pipe$great,
          $less$amp$amp$great: M.$less$amp$amp$great,
          orS: M.orS,
          andS: M.andS,
          whenS: M.whenS,
          branch: M.branch,
          ifS: M.ifS,
          fromOptionS: M.fromOptionS,
          anyS: M.anyS,
          allS: M.allS,
          whileS: M.whileS,
          bind: M.bind,
          MonadInfix: M.MonadInfix,
          $great$great$eq: M.$great$great$eq,
          $great$great$tilde: M.$great$great$tilde,
          $great$eq$great: M.$great$eq$great,
          join: M.join,
          forever: M.forever,
          sequenceM: M.sequenceM,
          mapM: M.mapM,
          mapM_: M.mapM_,
          alt: $$let.alt,
          AltInfix: $$let.AltInfix,
          empty: $$let.empty,
          map: $$let.map,
          replace: $$let.replace,
          $$void: $$let.$$void,
          $less$$great: $$let.$less$$great,
          $less$amp$great: $$let.$less$amp$great,
          $less$: $$let.$less$,
          $$great: $$let.$$great,
          apply: $$let.apply,
          applyFirst: $$let.applyFirst,
          applySecond: $$let.applySecond,
          liftA2: $$let.liftA2,
          $less$star$great: $$let.$less$star$great,
          $star$great: $$let.$star$great,
          $less$star: $$let.$less$star,
          $star$star: $$let.$star$star,
          liftA3: $$let.liftA3,
          liftA4: $$let.liftA4,
          liftA5: $$let.liftA5,
          merge: $$let.merge,
          pure: $$let.pure,
          when_: $$let.when_,
          unless: $$let.unless,
          AlternativeInfix: $$let.AlternativeInfix,
          $less$pipe$great: $$let.$less$pipe$great,
          $less$slash$great: $$let.$less$slash$great,
          some: $$let.some,
          many: $$let.many,
          optional: $$let.optional
        };
}

function Make2(X) {
  var pure = X.pure;
  var map_001 = X.map;
  var map = /* `Custom */[
    -198771759,
    map_001
  ];
  var M = Monad$CoreCategory.MakeCustom3({
        pure: pure,
        bind: X.bind,
        map: map,
        replace: /* Derived */-684824643,
        apply: /* Derived */-684824643,
        liftA2: /* Derived */-684824643,
        applyFirst: /* Derived */-684824643,
        applySecond: /* Derived */-684824643,
        select: /* Derived */-684824643,
        join: /* Derived */-684824643
      });
  var map_001$1 = M.map;
  var map$1 = /* `Custom */[
    -198771759,
    map_001$1
  ];
  var $$let = Alternative$CoreCategory.MakeCustom3({
        alt: X.alt,
        empty: X.empty,
        pure: pure,
        apply: M.apply,
        map: map$1,
        replace: /* Derived */-684824643,
        liftA2: /* Derived */-684824643,
        applyFirst: /* Derived */-684824643,
        applySecond: /* Derived */-684824643
      });
  return {
          FunctorInfix: M.FunctorInfix,
          ApplyInfix: M.ApplyInfix,
          select: M.select,
          SelectiveInfix: M.SelectiveInfix,
          $less$star$question: M.$less$star$question,
          $less$pipe$pipe$great: M.$less$pipe$pipe$great,
          $less$amp$amp$great: M.$less$amp$amp$great,
          orS: M.orS,
          andS: M.andS,
          whenS: M.whenS,
          branch: M.branch,
          ifS: M.ifS,
          fromOptionS: M.fromOptionS,
          anyS: M.anyS,
          allS: M.allS,
          whileS: M.whileS,
          bind: M.bind,
          MonadInfix: M.MonadInfix,
          $great$great$eq: M.$great$great$eq,
          $great$great$tilde: M.$great$great$tilde,
          $great$eq$great: M.$great$eq$great,
          join: M.join,
          forever: M.forever,
          sequenceM: M.sequenceM,
          mapM: M.mapM,
          mapM_: M.mapM_,
          alt: $$let.alt,
          AltInfix: $$let.AltInfix,
          empty: $$let.empty,
          map: $$let.map,
          replace: $$let.replace,
          $$void: $$let.$$void,
          $less$$great: $$let.$less$$great,
          $less$amp$great: $$let.$less$amp$great,
          $less$: $$let.$less$,
          $$great: $$let.$$great,
          apply: $$let.apply,
          applyFirst: $$let.applyFirst,
          applySecond: $$let.applySecond,
          liftA2: $$let.liftA2,
          $less$star$great: $$let.$less$star$great,
          $star$great: $$let.$star$great,
          $less$star: $$let.$less$star,
          $star$star: $$let.$star$star,
          liftA3: $$let.liftA3,
          liftA4: $$let.liftA4,
          liftA5: $$let.liftA5,
          merge: $$let.merge,
          pure: $$let.pure,
          when_: $$let.when_,
          unless: $$let.unless,
          AlternativeInfix: $$let.AlternativeInfix,
          $less$pipe$great: $$let.$less$pipe$great,
          $less$slash$great: $$let.$less$slash$great,
          some: $$let.some,
          many: $$let.many,
          optional: $$let.optional
        };
}

function Make1(X) {
  var pure = X.pure;
  var map_001 = X.map;
  var map = /* `Custom */[
    -198771759,
    map_001
  ];
  var M = Monad$CoreCategory.MakeCustom3({
        pure: pure,
        bind: X.bind,
        map: map,
        replace: /* Derived */-684824643,
        apply: /* Derived */-684824643,
        liftA2: /* Derived */-684824643,
        applyFirst: /* Derived */-684824643,
        applySecond: /* Derived */-684824643,
        select: /* Derived */-684824643,
        join: /* Derived */-684824643
      });
  var map_001$1 = M.map;
  var map$1 = /* `Custom */[
    -198771759,
    map_001$1
  ];
  var $$let = Alternative$CoreCategory.MakeCustom3({
        alt: X.alt,
        empty: X.empty,
        pure: pure,
        apply: M.apply,
        map: map$1,
        replace: /* Derived */-684824643,
        liftA2: /* Derived */-684824643,
        applyFirst: /* Derived */-684824643,
        applySecond: /* Derived */-684824643
      });
  return {
          FunctorInfix: M.FunctorInfix,
          ApplyInfix: M.ApplyInfix,
          select: M.select,
          SelectiveInfix: M.SelectiveInfix,
          $less$star$question: M.$less$star$question,
          $less$pipe$pipe$great: M.$less$pipe$pipe$great,
          $less$amp$amp$great: M.$less$amp$amp$great,
          orS: M.orS,
          andS: M.andS,
          whenS: M.whenS,
          branch: M.branch,
          ifS: M.ifS,
          fromOptionS: M.fromOptionS,
          anyS: M.anyS,
          allS: M.allS,
          whileS: M.whileS,
          bind: M.bind,
          MonadInfix: M.MonadInfix,
          $great$great$eq: M.$great$great$eq,
          $great$great$tilde: M.$great$great$tilde,
          $great$eq$great: M.$great$eq$great,
          join: M.join,
          forever: M.forever,
          sequenceM: M.sequenceM,
          mapM: M.mapM,
          mapM_: M.mapM_,
          alt: $$let.alt,
          AltInfix: $$let.AltInfix,
          empty: $$let.empty,
          map: $$let.map,
          replace: $$let.replace,
          $$void: $$let.$$void,
          $less$$great: $$let.$less$$great,
          $less$amp$great: $$let.$less$amp$great,
          $less$: $$let.$less$,
          $$great: $$let.$$great,
          apply: $$let.apply,
          liftA2: $$let.liftA2,
          applyFirst: $$let.applyFirst,
          applySecond: $$let.applySecond,
          $less$star$great: $$let.$less$star$great,
          $star$great: $$let.$star$great,
          $less$star: $$let.$less$star,
          $star$star: $$let.$star$star,
          liftA3: $$let.liftA3,
          liftA4: $$let.liftA4,
          liftA5: $$let.liftA5,
          merge: $$let.merge,
          pure: $$let.pure,
          when_: $$let.when_,
          unless: $$let.unless,
          AlternativeInfix: $$let.AlternativeInfix,
          $less$pipe$great: $$let.$less$pipe$great,
          $less$slash$great: $$let.$less$slash$great,
          some: $$let.some,
          many: $$let.many,
          optional: $$let.optional
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
/* Monad-CoreCategory Not a pure module */
