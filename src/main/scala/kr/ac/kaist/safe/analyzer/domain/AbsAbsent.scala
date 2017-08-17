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

////////////////////////////////////////////////////////////////////////////////
// concrete absent type
////////////////////////////////////////////////////////////////////////////////
sealed abstract class Absent
case object Absent extends Absent

////////////////////////////////////////////////////////////////////////////////
// absent abstract domain
////////////////////////////////////////////////////////////////////////////////
trait AbsAbsent extends AbsDomain[Absent, AbsAbsent]
trait AbsAbsentUtil extends AbsDomainUtil[Absent, AbsAbsent]

////////////////////////////////////////////////////////////////////////////////
// default absent abstract domain
////////////////////////////////////////////////////////////////////////////////
object DefaultAbsent extends AbsAbsentUtil {
  case object Bot extends Dom
  case object Top extends Dom

  def alpha(abs: Absent): AbsAbsent = Top

  abstract class Dom extends AbsAbsent {
    def gamma: ConSet[Absent] = this match {
      case Bot => ConFin()
      case Top => ConFin(Absent)
    }

    def isBottom: Boolean = this == Bot
    def isTop: Boolean = this == Top

    def getSingle: ConSingle[Absent] = this match {
      case Bot => ConZero()
      case Top => ConOne(Absent)
    }

    def <=(that: AbsAbsent): Boolean = (this, check(that)) match {
      case (Top, Bot) => false
      case _ => true
    }

    def +(that: AbsAbsent): AbsAbsent = (this, check(that)) match {
      case (Bot, Bot) => Bot
      case _ => Top
    }

    def <>(that: AbsAbsent): AbsAbsent = (this, check(that)) match {
      case (Top, Top) => Top
      case _ => Bot
    }

    override def toString: String = this match {
      case Top => "Top(absent)"
      case Bot => "⊥(absent)"
    }
  }
}
