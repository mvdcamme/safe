var __result1 = true;
  try
{    var n = Number.prototype.toString(37);
var __result1 = false;
}
  catch (e)
{    }
var __expect1 = true;

var __result2 = true;
  try
{    var n = (new Number()).toString(37);
var __result2 = false;
}
  catch (e)
{    }
var __expect2 = true;

var __result3 = true;
  try
{    var n = (new Number(0)).toString(37);
var __result3 = false;
}
  catch (e)
{    }
var __expect3 = true;

var __result4 = true;
  try
{    var n = (new Number(- 1)).toString(37);
var __result4 = false;
}
  catch (e)
{    }
var __expect4 = true;

var __result5 = true;
  try
{    var n = (new Number(1)).toString(37);
     var __result5 = false;
}
  catch (e)
{    }
var __expect5 = true;

var __result6 = true;
  try
{    var n = (new Number(Number.NaN)).toString(37);
var __result6 = false;
}
  catch (e)
{    }
var __expect6 = true;

var __result7 = true;
  try
{    var n = (new Number(Number.POSITIVE_INFINITY)).toString(37);
var __result7 = false;
}
  catch (e)
{    }
var __expect7 = true;

var __result8 = true;
  try
{    var n = (new Number(Number.NEGATIVE_INFINITY)).toString(37);
var __result8 = false;
}
  catch (e)
{    }
var __expect8 = true;
