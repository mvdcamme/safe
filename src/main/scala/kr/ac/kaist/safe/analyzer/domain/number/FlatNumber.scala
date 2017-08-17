/**
 * *****************************************************************************
 * Copyright (c) 2016-2017, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.analyzer.domain

import kr.ac.kaist.safe.analyzer.domain.Utils._
import Double._

// flat number abstract domain
object FlatNumber extends AbsNumberUtil {
  case object Top extends Dom
  case object Bot extends Dom
  case class Const(value: Double) extends Dom
  lazy val Inf = Top
  lazy val PosInf = Const(PositiveInfinity)
  lazy val NegInf = Const(NegativeInfinity)
  lazy val NaN = Const(Double.NaN)
  lazy val UInt = Top
  lazy val NUInt = Top

  def alpha(num: Num): Dom = Const(num.num)

  sealed abstract class Dom extends AbsNumber {
    override def equals(other: Any): Boolean = other match {
      case (right: Dom) => (this, right) match {
        case (_: Bot.type, _: Bot.type) | (_: Top.type, _: Top.type) => true
        case (lc: Const, rc: Const) => {
          val l = lc.value
          val r = rc.value
          if (l.isNaN && r.isNaN) true
          else if ((isNegZero(l) && !isNegZero(r)) ||
            (!isNegZero(l) && isNegZero(r))) false
          else l == r
        }
        case _ => false
      }
      case _ => false
    }

    def gamma: ConSet[Num] = this match {
      case Bot => ConFin()
      case Const(v) => ConFin(v)
      case Top => ConInf()
    }

    def isBottom: Boolean = this == Bot
    def isTop: Boolean = this == Top

    def getSingle: ConSingle[Num] = this match {
      case Bot => ConZero()
      case Const(v) => ConOne(v)
      case Top => ConMany()
    }

    override def toString: String = this match {
      case Bot => "⊥(number)"
      case Const(v) => v.toString
      case Top => "Top(number)"
    }

    def <=(that: AbsNumber): Boolean = (this, check(that)) match {
      case (Bot, _) => true
      case (_, Top) => true
      case (left, right) if left == right => true
      case _ => false
    }

    def +(that: AbsNumber): Dom = (this, check(that)) match {
      case (left, right) if left <= right => right
      case (left, right) if right <= left => left
      case _ => Top
    }

    def <>(that: AbsNumber): Dom = (this, check(that)) match {
      case (left, right) if left <= right => left
      case (left, right) if right <= left => right
      case _ => Bot
    }

    def <(that: AbsNumber): AbsBool = (this, check(that)) match {
      case (Bot, _) | (_, Bot) => AbsBool.Bot
      case (Const(n1), Const(n2)) => AbsBool(n1 < n2)
      case _ => AbsBool.Top
    }

    def ===(that: AbsNumber): AbsBool = (this, check(that)) match {
      case (Bot, _) | (_, Bot) => AbsBool.Bot
      case (Const(n1), Const(n2)) => AbsBool(n1 == n2)
      case _ => AbsBool.Top
    }

    /* Operators */
    private def toString(m: Double): String = {
      // 5. Otherwise, let n, k, and s be integers such that k >= 1,
      //    10^(k-1) <= s < 10^k, the Number value for s * 10^(n-k) is m,
      //    and k is as small as possible.
      //    Note that k is the number of digits in the decimal representation of s,
      //    that s is not divisible by 10, and that the least significant digit of s
      //    is not necessarily uniquely determined by these criteria.
      var s = BigDecimal(m)
      var n = 0
      while (s % 10 == BigDecimal(0) || s % 1 != BigDecimal(0)) {
        if (s % 10 == BigDecimal(0)) {
          s /= 10
          n += 1
        } else {
          s *= 10
          n -= 1
        }
      }
      var sLong = s.toLong
      var k = 0
      while (s >= BigDecimal(1)) {
        s /= 10
        k += 1
      }
      n += k
      def getStr(number: Long): String = {
        var str = ""
        var sLong = number
        while (sLong > 0) {
          str += (sLong % 10).toString
          sLong /= 10
        }
        str.reverse
      }
      def getSign(n: Int): Char = {
        if (n - 1 > 0) '+'
        else '-'
      }
      if (k <= n && n <= 21) {
        // 6. If k <= n <= 21, return the String consisting of the k digits of the decimal representation of s
        //    (in order, with no leading zeroes), followed by n-k occurrences of the character '0'.
        getStr(sLong) + ("0" * (n - k))
      } else if (0 < n && n <= 21) {
        // 7. If 0 < n <= 21, return the String consisting of the most significant n digits of
        //    the decimal representation of s, followed by a decimal point '.',
        //    followed by the remaining k-n digits of the decimal representation of s.
        val str = getStr(sLong)
        str.substring(0, n) + '.' + str.substring(n)
      } else if (-6 < n && n <= 0) {
        // 8. If -6 < n <= 0, return the String consisting of the character '0', followed by a decimal point '.',
        //    followed by -n occurrences of the character '0', followed by the k digits of the decimal representation of s.
        "0." + ("0" * (-n)) + getStr(sLong)
      } else if (k == 1) {
        // 9. Otherwise, if k = 1, return the String consisting of the single digit of s,
        //    followed by lowercase character 'e', followed by a plus sign '+' or minus sign '-'
        //    according to whether n-1 is positive or negative, followed by the decimal representation of
        //    the integer abs(n-1) (with no leading zeroes).
        getStr(sLong) + "e" + getSign(n) + math.abs(n - 1).toString
      } else {
        // 10. Return the String consisting of the most significant digit of the decimal representation of s,
        //     followed by a decimal point '.', followed by the remaining k-1 digits of the decimal representation of s,
        //     followed by the lowercase character 'e', followed by a plus sign '+' or minus sign '-' according to
        //     whether n-1 is positive or negative, followed by the decimal representation of the integer abs(n-1) (with no leading zeroes).
        val str = getStr(sLong)
        str.substring(0, 1) + '.' + str.substring(1) + 'e' + getSign(n) + math.abs(n - 1).toString
      }
    }

    // 9.8.1 ToString Applied to the Number Type
    def toAbsString: AbsString = this match {
      case Bot => AbsString.Bot
      case Const(0) => AbsString("0")
      case Const(n) if n < 0 => AbsString("-") concat alpha(-n).toAbsString
      case Const(n) if n.isNaN => AbsString("NaN")
      case Const(PositiveInfinity) => AbsString("Infinity")
      case Const(n) => AbsString(toString(n))
      case Top => AbsString.Number
    }

    // 9.2 ToBoolean
    def toAbsBoolean: AbsBool = this match {
      case Bot => AbsBool.Bot
      case Const(n) if n == 0.0 || n.isNaN => AbsBool.False
      case Const(_) => AbsBool.True
      case _ => AbsBool.Top
    }

    // 9.4 ToInteger
    def toInteger: Dom = this match {
      case Bot => Bot
      // 2. If number is NaN, return +0.
      case Const(n) if n.isNaN => Const(0.0)
      // 3. If number is +0, -0, +Infinity, or Infinity, return number.
      case Const(0) | Const(PositiveInfinity) | Const(NegativeInfinity) => this
      // 4. Return the result of computing sign(number) * floor(abs(number)).
      case Const(n) => Const(math.signum(n) * math.floor(math.abs(n)))
      // other cases
      case _ => Top
    }

    private def modulo(posInt: Long, bound: Long): Long = {
      val value = posInt % bound
      if (value < 0) value + bound
      else value
    }

    // 9.5 ToInt32: (Signed 32 Bit Integer)
    def toInt32: Dom = {
      def helper(number: Double): Long = {
        val bound = 0x100000000L // 2^32
        // 3. Let posInt be sign(number) * floor(abs(number)).
        val posInt: Long = (math.signum(number) * math.floor(math.abs(number))).toLong
        // 4. Let int32bit be posInt modulo 2^32;
        val int32bit = modulo(posInt, bound)
        // 5. If int32bit is greater than or equal to 2^31, return int32bit - 2^32, otherwise return int32bit.
        if (int32bit > bound / 2) int32bit - bound
        else int32bit
      }
      this match {
        case Bot => Bot
        // 2. If number is NaN, +0, -0, +Infinity, or Infinity, return +0.
        case Const(n) if n.isNaN || n == 0 || n == PositiveInfinity || n == NegativeInfinity => Const(0)
        // by helper
        case Const(n) => alpha(helper(n))
        // other cases
        case _ => Top
      }
    }

    // 9.6 ToUint32: (Unsigned 32 Bit Integer)
    def toUInt32: Dom = {
      def helper(number: Double): Long = {
        val bound = 0x100000000L // 2^32
        // 3. Let posInt be sign(number) * floor(abs(number)).
        val posInt: Long = (math.signum(number) * math.floor(math.abs(number))).toLong
        // 4. Let int32bit be posInt modulo 2^32;
        val int32bit = modulo(posInt, bound)
        // 5. Return int32bit.
        int32bit
      }
      this match {
        case Bot => Bot
        // 2. If number is NaN, +0, -0, +Infinity, or Infinity, return +0.
        case Const(n) if n.isNaN || n == 0 || n == PositiveInfinity || n == NegativeInfinity => Const(0.0)
        // by helper
        case Const(n) => Const(helper(n))
        // other cases
        case _ => Top
      }
    }

    // 9.7 ToUint16: (Unsigned 16 Bit Integer)
    def toUInt16: Dom = {
      def helper(number: Double): Long = {
        val bound = 0x10000L // 2^16
        // 3. Let posInt be sign(number) * floor(abs(number)).
        val posInt: Long = (math.signum(number) * math.floor(math.abs(number))).toLong
        // 4. Let int16bit be posInt modulo 2^16;
        val int16bit = modulo(posInt, bound)
        // 5. Return int16bit.
        int16bit
      }
      this match {
        case Bot => Bot
        // 2. If number is NaN, +0, -0, +Infinity, or Infinity, return +0.
        case Const(n) if n.isNaN || n == 0 || n == PositiveInfinity || n == NegativeInfinity => Const(0.0)
        // by helper
        case Const(n) => Const(helper(n))
        // other cases
        case _ => Top
      }
    }

    // 9.12 The SameValue Algorithm
    def sameValue(that: AbsNumber): AbsBool = (this, check(that)) match {
      case (Bot, _) | (_, Bot) => AbsBool.Bot
      case (left: Const, right: Const) => AbsBool(left == right)
      case _ => AbsBool.Top
    }

    // 11.4.7 Unary-Operator
    def negate: Dom = this match {
      case Bot => Bot
      case Const(n) => alpha(-n)
      case Top => Top
    }

    // 15.8.2.1 abs (x)
    def abs: Dom = this match {
      case Bot => Bot
      case Const(n) => alpha(math.abs(n))
      case Top => Top
    }

    // 15.8.2.2 acos (x)
    def acos: Dom = this match {
      case Bot => Bot
      case Const(n) => alpha(math.acos(n))
      case Top => Top
    }

    // 15.8.2.3 asin (x)
    def asin: Dom = this match {
      case Bot => Bot
      case Const(n) => alpha(math.asin(n))
      case Top => Top
    }

    // 15.8.2.4 atan (x)
    def atan: Dom = this match {
      case Bot => Bot
      case Const(n) => alpha(math.atan(n))
      case Top => Top
    }

    // 15.8.2.5 atan2 (y, x)
    def atan2(that: AbsNumber): Dom = (this, check(that)) match {
      case (Bot, _) | (_, Bot) => Bot
      case (Top, _) | (_, Top) => Top
      case (Const(y), Const(x)) => alpha(math.atan2(y, x))
    }

    // 15.8.2.6 ceil (x)
    def ceil: Dom = this match {
      case Bot => Bot
      case Const(n) => alpha(math.ceil(n))
      case Top => Top
    }

    // 15.8.2.7 cos (x)
    def cos: Dom = this match {
      case Bot => Bot
      case Const(n) => alpha(math.cos(n))
      case Top => Top
    }

    // 15.8.2.8 exp (x)
    def exp: Dom = this match {
      case Bot => Bot
      case Const(n) => alpha(math.exp(n))
      case Top => Top
    }

    // 15.8.2.9 floor (x)
    def floor: Dom = this match {
      case Bot => Bot
      case Const(n) => alpha(math.floor(n))
      case Top => Top
    }

    // 15.8.2.10 log (x)
    def log: Dom = this match {
      case Bot => Bot
      case Const(n) => alpha(math.log(n))
      case Top => Top
    }

    // 15.8.2.13 pow (x, y)
    def pow(that: AbsNumber): Dom = (this, check(that)) match {
      case (Bot, _) | (_, Bot) => Bot
      case (Top, _) | (_, Top) => Top
      case (Const(x), Const(y)) => alpha(math.pow(x, y))
    }

    // 15.8.2.15 round (x)
    def round: Dom = this match {
      case Bot => Bot
      case Const(PositiveInfinity) => PosInf
      case Const(NegativeInfinity) => NegInf
      case Const(n) if n.isNaN => NaN
      case Const(n) => alpha(math.round(n))
      case Top => Top
    }

    // 15.8.2.16 sin (x)
    def sin: Dom = this match {
      case Bot => Bot
      case Const(n) => alpha(math.sin(n))
      case Top => Top
    }

    // 15.8.2.17 sqrt (x)
    def sqrt: Dom = this match {
      case Bot => Bot
      case Const(n) => alpha(math.sqrt(n))
      case Top => Top
    }

    // 15.8.2.18 tan (x)
    def tan: Dom = this match {
      case Bot => Bot
      case Const(n) => alpha(math.tan(n))
      case Top => Top
    }

    // 11.4.8 Bitwise NOT Operator ( ~ )
    // 1. Let expr be the result of evaluating UnaryExpression.
    // 2. Let oldValue be ToInt32(GetValue(expr)).
    def bitNegate: Dom = toInt32 match {
      case Bot => Bot
      case Const(n) => alpha(~(n.toLong.toInt))
      case Top => Top
    }

    private def binaryBitwiseOp(left: Dom, right: Dom)(op: (Int, Int) => Int): Dom = (left.toInt32, right.toInt32) match {
      case (Const(l), Const(r)) => alpha(op(l.toLong.toInt, r.toLong.toInt))
      case _ => Top
    }

    // 11.10 BinaryBitwiseOperators
    def bitOr(that: AbsNumber): Dom = binaryBitwiseOp(this, check(that))(_ | _)
    def bitAnd(that: AbsNumber): Dom = binaryBitwiseOp(this, check(that))(_ & _)
    def bitXor(that: AbsNumber): Dom = binaryBitwiseOp(this, check(that))(_ ^ _)

    private def binaryShiftOp(
      left: Dom,
      right: Dom,
      signed: Boolean = true
    )(op: (Int, Int) => Long): Dom = (left.toUInt32, right.toUInt32) match {
      case (Const(l), Const(r)) =>
        val bound = 0x100000000L
        val l32 = l.toLong.toInt
        val r32 = (r.toLong & 0x1F).toInt
        val result = op(l32, r32)
        if (!signed && result < 0) alpha(bound + result.toLong)
        else alpha(result)
      case _ => Top
    }

    // 11.7.1 The Left Shift Operator ( << )
    def bitLShift(shift: AbsNumber): Dom = binaryShiftOp(this, check(shift))(_ << _)
    // 11.7.2 The Signed Right Shift Operator ( >> )
    def bitRShift(shift: AbsNumber): Dom = binaryShiftOp(this, check(shift))(_ >> _)
    // 11.7.3 The Unsigned Right Shift Operator ( >>> )
    def bitURShift(shift: AbsNumber): Dom = binaryShiftOp(this, check(shift), false)(_ >>> _)

    // 11.6.3 Applying the Additive Operators to Numbers
    def add(that: AbsNumber): Dom = (this, check(that)) match {
      case (Bot, _) | (_, Bot) => Bot
      case (Const(l), Const(r)) => Const(l + r)
      case _ => Top
    }

    def sub(that: AbsNumber): Dom = this add (that.negate)

    def mul(that: AbsNumber): Dom = (this, check(that)) match {
      case (Bot, _) | (_, Bot) => Bot
      case (Const(l), Const(r)) => Const(l * r)
      case _ => Top
    }

    def div(that: AbsNumber): Dom = (this, check(that)) match {
      case (Bot, _) | (_, Bot) => Bot
      case (Const(l), Const(r)) => Const(l / r)
      case _ => Top
    }

    def mod(that: AbsNumber): Dom = (this, check(that)) match {
      case (Bot, _) | (_, Bot) => Bot
      case (Const(l), Const(r)) => Const(l % r)
      case _ => Top
    }
  }

  private def isNegZero(v: Double): Boolean = 1 / v == NegativeInfinity
}
