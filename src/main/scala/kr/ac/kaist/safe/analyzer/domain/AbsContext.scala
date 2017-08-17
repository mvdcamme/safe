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
import kr.ac.kaist.safe.analyzer.models.builtin.BuiltinGlobal
import kr.ac.kaist.safe.LINE_SEP
import kr.ac.kaist.safe.util._
import scala.collection.immutable.{ HashMap, HashSet }
import kr.ac.kaist.safe.nodes.cfg._

/* 10.3 Execution Contexts */

////////////////////////////////////////////////////////////////////////////////
// concrete execution context type
////////////////////////////////////////////////////////////////////////////////
trait Context // TODO

////////////////////////////////////////////////////////////////////////////////
// execution context abstract domain
////////////////////////////////////////////////////////////////////////////////
trait AbsContext extends AbsDomain[Context, AbsContext] {
  // lookup
  def apply(loc: Loc): Option[AbsLexEnv]
  def apply(locSet: Set[Loc]): AbsLexEnv
  def apply(locSet: AbsLoc): AbsLexEnv
  def getOrElse(loc: Loc, default: AbsLexEnv): AbsLexEnv
  def getOrElse[T](loc: Loc)(default: T)(f: AbsLexEnv => T): T

  // context update
  def weakUpdate(loc: Loc, env: AbsLexEnv): AbsContext
  def update(loc: Loc, env: AbsLexEnv): AbsContext

  // remove location
  def remove(loc: Loc): AbsContext

  // substitute locR by locO
  def subsLoc(locR: Recency, locO: Recency): AbsContext

  def oldify(loc: Loc): AbsContext

  def domIn(loc: Loc): Boolean

  def isBottom: Boolean

  def setOldASiteSet(old: OldASiteSet): AbsContext

  def setThisBinding(thisBinding: AbsValue): AbsContext

  def getMap: Map[Loc, AbsLexEnv]

  def old: OldASiteSet

  def thisBinding: AbsValue

  def toStringLoc(loc: Loc): Option[String]

  // delete
  def delete(loc: Loc, str: String): (AbsContext, AbsBool)

  // pure local environment
  def pureLocal: AbsLexEnv
  def subsPureLocal(env: AbsLexEnv): AbsContext

  // location concrete check
  def isConcrete(loc: Loc): Boolean
}

trait AbsContextUtil extends AbsDomainUtil[Context, AbsContext] {
  val Empty: AbsContext
  def apply(
    map: Map[Loc, AbsLexEnv],
    absSet: Set[Concrete],
    old: OldASiteSet,
    thisBinding: AbsValue
  ): AbsContext
}

////////////////////////////////////////////////////////////////////////////////
// default execution context abstract domain
////////////////////////////////////////////////////////////////////////////////
object DefaultContext extends AbsContextUtil {
  private val EmptyMap: Map[Loc, AbsLexEnv] = HashMap()

  case object Bot extends Dom
  case object Top extends Dom
  case class CtxMap(
    // TODO val varEnv: LexEnv // VariableEnvironment
    val map: Map[Loc, AbsLexEnv],
    val absSet: Set[Concrete],
    override val old: OldASiteSet,
    override val thisBinding: AbsValue // ThisBinding
  ) extends Dom
  lazy val Empty: AbsContext =
    CtxMap(EmptyMap, HashSet(), OldASiteSet.Empty, AbsLoc(BuiltinGlobal.loc))

  def alpha(ctx: Context): AbsContext = Top // TODO more precise

  def apply(
    map: Map[Loc, AbsLexEnv],
    absSet: Set[Concrete],
    old: OldASiteSet,
    thisBinding: AbsValue
  ): AbsContext = CtxMap(map, absSet, old, thisBinding)

  sealed abstract class Dom extends AbsContext {
    def gamma: ConSet[Context] = ConInf() // TODO more precise

    def getSingle: ConSingle[Context] = ConMany() // TODO more precise

    def isBottom: Boolean = this == Bot
    def isTop: Boolean = this == Top

    def <=(that: AbsContext): Boolean = (this, check(that)) match {
      case (Bot, _) => true
      case (_, Bot) => false
      case (_, Top) => true
      case (Top, _) => false
      case (CtxMap(thisMap, thisASet, thisOld, thisThis),
        CtxMap(thatMap, thatASet, thatOld, thatThis)) => {
        val mapB =
          if (thisMap.isEmpty) true
          else if (thatMap.isEmpty) false
          else thisMap.forall {
            case (loc, thisEnv) => thatMap.get(loc) match {
              case None => false
              case Some(thatEnv) => thisEnv <= thatEnv
            }
          }
        val asetB = thisASet subsetOf thatASet
        val oldB = thisOld <= thatOld
        val thisB = thisThis <= thatThis
        mapB && asetB && oldB && thisB
      }
    }

    def +(that: AbsContext): AbsContext = (this, check(that)) match {
      case (Bot, _) => that
      case (_, Bot) => this
      case (Top, _) | (_, Top) => Top
      case (CtxMap(thisMap, thisASet, thisOld, thisThis),
        CtxMap(thatMap, thatASet, thatOld, thatThis)) => {
        if (this eq that) this
        else {
          val newMap = thatMap.foldLeft(thisMap) {
            case (m, (loc, thatEnv)) => m.get(loc) match {
              case None => m + (loc -> thatEnv)
              case Some(thisEnv) =>
                m + (loc -> (thisEnv + thatEnv))
            }
          }
          val newASet = thisASet ++ thatASet
          val newOld = thisOld + thatOld
          val newThis = thisThis + thatThis
          CtxMap(newMap, newASet, newOld, newThis)
        }
      }
    }

    def <>(that: AbsContext): AbsContext = (this, check(that)) match {
      case (Bot, _) | (_, Bot) => Bot
      case (Top, _) => that
      case (_, Top) => this
      case (CtxMap(thisMap, thisASet, thisOld, thisThis),
        CtxMap(thatMap, thatASet, thatOld, thatThis)) => {
        if (thisMap eq thatMap) this
        else {
          val locSet = thisMap.keySet intersect thatMap.keySet
          val newMap = locSet.foldLeft(EmptyMap) {
            case (m, loc) => {
              val thisEnv = thisMap(loc)
              val thatEnv = thatMap(loc)
              m + (loc -> (thisEnv <> thatEnv))
            }
          }
          val newASet = thisASet intersect thatASet
          val newOld = thisOld <> thatOld
          val newThis = thisThis <> thatThis
          CtxMap(newMap, newASet, newOld, newThis)
        }
      }
    }

    def apply(loc: Loc): Option[AbsLexEnv] = this match {
      case Bot => None
      case Top => Some(AbsLexEnv.Top)
      case CtxMap(map, _, _, _) => map.get(loc)
    }

    def apply(locSet: Set[Loc]): AbsLexEnv = locSet.foldLeft(AbsLexEnv.Bot) {
      case (envRec, loc) => envRec + getOrElse(loc, AbsLexEnv.Bot)
    }

    def apply(locSet: AbsLoc): AbsLexEnv = locSet.foldLeft(AbsLexEnv.Bot) {
      case (envRec, loc) => envRec + getOrElse(loc, AbsLexEnv.Bot)
    }

    def getOrElse(loc: Loc, default: AbsLexEnv): AbsLexEnv =
      this(loc) match {
        case Some(env) => env
        case None => default
      }

    def getOrElse[T](loc: Loc)(default: T)(f: AbsLexEnv => T): T = {
      this(loc) match {
        case Some(env) => f(env)
        case None => default
      }
    }

    private def weakUpdated(m: Map[Loc, AbsLexEnv], loc: Loc, newEnv: AbsLexEnv): Map[Loc, AbsLexEnv] =
      m.get(loc) match {
        case Some(oldEnv) => m.updated(loc, oldEnv + newEnv)
        case None => m.updated(loc, newEnv)
      }

    def weakUpdate(loc: Loc, env: AbsLexEnv): AbsContext = this match {
      case Bot => Bot
      case Top => Top
      case CtxMap(map, aset, old, thisBinding) =>
        CtxMap(weakUpdated(map, loc, env), aset, old, thisBinding)
    }

    def update(loc: Loc, env: AbsLexEnv): AbsContext = this match {
      case Bot => Bot
      case Top => Top
      case cmap @ CtxMap(map, aset, _, _) => {
        if (isConcrete(loc)) {
          cmap.copy(map = map.updated(loc, env))
        } else {
          cmap.copy(map = weakUpdated(map, loc, env))
        }
      }
    }

    def remove(loc: Loc): AbsContext = this match {
      case Bot => Bot
      case Top => Top
      case CtxMap(map, aset, old, thisBinding) => CtxMap(map - loc, loc match {
        case locC @ Concrete(_) => aset - locC
        case _ => aset
      }, old, thisBinding)
    }

    def subsLoc(locR: Recency, locO: Recency): AbsContext = this match {
      case Bot => Bot
      case Top => Top
      case CtxMap(map, aset, old, thisBinding) => {
        val newMap = map.foldLeft(EmptyMap) {
          case (m, (loc, env)) =>
            m + (loc -> env.subsLoc(locR, locO))
        }
        val newOld = old.subsLoc(locR, locO)
        val newThis = thisBinding.subsLoc(locR, locO)
        CtxMap(newMap, aset, newOld, newThis)
      }
    }

    def oldify(loc: Loc): AbsContext = loc match {
      case locR @ Recency(subLoc, Recent) => this match {
        case Bot => Bot
        case Top => Top
        case CtxMap(map, _, _, _) => {
          val locO = Recency(subLoc, Old)
          val newCtx = if (this domIn locR) {
            update(locO, getOrElse(locR, AbsLexEnv.Bot)).remove(locR)
          } else this
          newCtx.subsLoc(locR, locO)
        }
      }
      case locC @ Concrete(_) => this match {
        case Bot => Bot
        case Top => Top
        case cmap @ CtxMap(map, aset, _, _) => map contains locC match {
          case true => cmap.copy(absSet = aset + locC)
          case false => cmap
        }
      }
      case _ => this
    }

    def domIn(loc: Loc): Boolean = this match {
      case Bot => false
      case Top => true
      case CtxMap(map, _, _, _) => map.contains(loc)
    }

    def setOldASiteSet(old: OldASiteSet): AbsContext = this match {
      case Bot => Bot
      case Top => Top
      case cmap @ CtxMap(_, _, _, _) => cmap.copy(old = old)
    }

    def setThisBinding(thisBinding: AbsValue): AbsContext = this match {
      case Bot => Bot
      case Top => Top
      case cmap @ CtxMap(_, _, _, _) => cmap.copy(thisBinding = thisBinding)
    }

    def getMap: Map[Loc, AbsLexEnv] = this match {
      case Bot => HashMap()
      case Top => HashMap() // TODO it is not sound
      case CtxMap(map, _, _, _) => map
    }

    def old: OldASiteSet = this match {
      case Bot => OldASiteSet.Bot
      case Top => OldASiteSet.Bot // TODO it is not sound
      case CtxMap(_, _, old, _) => old
    }

    def thisBinding: AbsValue = this match {
      case Bot => AbsValue.Bot
      case Top => AbsValue.Top
      case CtxMap(_, _, _, thisBinding) => thisBinding
    }

    override def toString: String = {
      buildString(_ => true).toString
    }

    private def buildString(filter: Loc => Boolean): String = this match {
      case Bot => "⊥AbsContext"
      case Top => "Top"
      case CtxMap(map, aset, old, thisBinding) => {
        val s = new StringBuilder
        val sortedSeq =
          map.toSeq.filter { case (loc, _) => filter(loc) }
            .sortBy { case (loc, _) => loc }
        sortedSeq.map {
          case (loc, env) => s.append(toStringLoc(loc, env, isConcrete(loc))).append(LINE_SEP)
        }
        s.append(s"this: $thisBinding")
        s.toString
      }
    }

    def toStringLoc(loc: Loc): Option[String] = this match {
      case Bot => None
      case Top => Some(toStringLoc(loc, AbsLexEnv.Top, true))
      case CtxMap(map, aset, _, _) => map.get(loc).map(toStringLoc(loc, _, isConcrete(loc)))
    }

    private def toStringLoc(loc: Loc, env: AbsLexEnv, con: Boolean): String = {
      val s = new StringBuilder
      val keyStr = loc.toString + (loc match {
        case Concrete(_) => con match {
          case true => " -!> "
          case false => " -?> "
        }
        case _ => " -> "
      })
      s.append(keyStr)
      Useful.indentation(s, env.toString, keyStr.length)
      s.toString
    }

    ////////////////////////////////////////////////////////////////
    // delete
    ////////////////////////////////////////////////////////////////
    def delete(loc: Loc, str: String): (AbsContext, AbsBool) = {
      getOrElse(loc)((this, AbsBool.Bot))(_ => {
        val test = hasOwnProperty(loc, str)
        if (AbsBool.True <= test)
          (this, AbsBool.False)
        else
          (Bot, AbsBool.Bot)
      })
    }

    private def hasOwnProperty(loc: Loc, str: String): AbsBool = {
      (this.getOrElse(loc, AbsLexEnv.Bot).record.decEnvRec HasBinding str)
    }

    ////////////////////////////////////////////////////////////////
    // pure local environment
    ////////////////////////////////////////////////////////////////
    def pureLocal: AbsLexEnv =
      getOrElse(PredAllocSite.PURE_LOCAL, AbsLexEnv.Bot)
    def subsPureLocal(env: AbsLexEnv): AbsContext =
      update(PredAllocSite.PURE_LOCAL, env)

    ////////////////////////////////////////////////////////////////
    // location concrete check
    ////////////////////////////////////////////////////////////////
    def isConcrete(loc: Loc): Boolean = loc match {
      case locC @ Concrete(_) => this match {
        case Bot => true
        case Top => false
        case CtxMap(_, absSet, _, _) => !(absSet contains locC)
      }
      case Recency(_, Recent) => true
      case l if Loc.predConSet contains l => true
      case _ => false
    }
  }
}
