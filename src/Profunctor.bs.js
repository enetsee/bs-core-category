// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");

function MakeCustom3(X) {
  var dimap = X.dimap;
  var match = X.cmapFirst;
  var cmapFirst;
  var exit = 0;
  if (typeof match === "number" || match[0] !== -198771759) {
    exit = 1;
  } else {
    cmapFirst = match[1];
  }
  if (exit === 1) {
    cmapFirst = (function (t, f) {
        return Curry._3(dimap, t, f, (function (x) {
                      return x;
                    }));
      });
  }
  var match$1 = X.mapSecond;
  var mapSecond;
  var exit$1 = 0;
  if (typeof match$1 === "number" || match$1[0] !== -198771759) {
    exit$1 = 1;
  } else {
    mapSecond = match$1[1];
  }
  if (exit$1 === 1) {
    mapSecond = (function (t, f) {
        return Curry._3(dimap, t, (function (x) {
                      return x;
                    }), f);
      });
  }
  return {
          dimap: dimap,
          cmapFirst: cmapFirst,
          mapSecond: mapSecond
        };
}

function MakeCustom2(X) {
  var dimap = X.dimap;
  var cmapFirst = X.cmapFirst;
  var mapSecond = X.mapSecond;
  var cmapFirst$1;
  var exit = 0;
  if (typeof cmapFirst === "number" || cmapFirst[0] !== -198771759) {
    exit = 1;
  } else {
    cmapFirst$1 = cmapFirst[1];
  }
  if (exit === 1) {
    cmapFirst$1 = (function (t, f) {
        return Curry._3(dimap, t, f, (function (x) {
                      return x;
                    }));
      });
  }
  var mapSecond$1;
  var exit$1 = 0;
  if (typeof mapSecond === "number" || mapSecond[0] !== -198771759) {
    exit$1 = 1;
  } else {
    mapSecond$1 = mapSecond[1];
  }
  if (exit$1 === 1) {
    mapSecond$1 = (function (t, f) {
        return Curry._3(dimap, t, (function (x) {
                      return x;
                    }), f);
      });
  }
  return {
          dimap: dimap,
          cmapFirst: cmapFirst$1,
          mapSecond: mapSecond$1
        };
}

function Make3(X) {
  var dimap = X.dimap;
  var cmapFirst;
  var exit = 0;
  exit = 1;
  if (exit === 1) {
    cmapFirst = (function (t, f) {
        return Curry._3(dimap, t, f, (function (x) {
                      return x;
                    }));
      });
  }
  var mapSecond;
  var exit$1 = 0;
  exit$1 = 1;
  if (exit$1 === 1) {
    mapSecond = (function (t, f) {
        return Curry._3(dimap, t, (function (x) {
                      return x;
                    }), f);
      });
  }
  return {
          dimap: dimap,
          cmapFirst: cmapFirst,
          mapSecond: mapSecond
        };
}

function Make2(X) {
  var dimap = X.dimap;
  var cmapFirst;
  var exit = 0;
  exit = 1;
  if (exit === 1) {
    cmapFirst = (function (t, f) {
        return Curry._3(dimap, t, f, (function (x) {
                      return x;
                    }));
      });
  }
  var mapSecond;
  var exit$1 = 0;
  exit$1 = 1;
  if (exit$1 === 1) {
    mapSecond = (function (t, f) {
        return Curry._3(dimap, t, (function (x) {
                      return x;
                    }), f);
      });
  }
  return {
          dimap: dimap,
          cmapFirst: cmapFirst,
          mapSecond: mapSecond
        };
}

exports.MakeCustom2 = MakeCustom2;
exports.MakeCustom3 = MakeCustom3;
exports.Make2 = Make2;
exports.Make3 = Make3;
/* No side effect */
