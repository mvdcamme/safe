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

trait ConDomain[T, Self <: ConDomain[T, _]] extends Domain[Self]
trait ConDomainUtil[T, CON <: ConDomain[T, CON]] extends DomainUtil[CON]

////////////////////////////////////////////////////////////////////////////////
// concrete single domain
////////////////////////////////////////////////////////////////////////////////
sealed abstract class ConSingle[T] extends ConDomain[T, ConSingle[T]] {
  def isBottom: Boolean = this == ConZero[T]()
  def isTop: Boolean = this == ConMany()

  override def toString: String = this match {
    case ConZero() => "[]"
    case ConOne(t) => "[$t]"
    case ConMany() => "[< more than 2 values >]"
  }

  def <=(that: ConSingle[T]): Boolean = (this, that) match {
    case (ConZero(), _) | (_, ConMany()) => true
    case (ConOne(t), ConOne(u)) if t == u => true
    case _ => false
  }

  def +(that: ConSingle[T]): ConSingle[T] = (this, that) match {
    case (ConZero(), _) => that
    case (_, ConZero()) => this
    case (ConOne(t), ConOne(u)) if t == u => this
    case _ => ConMany[T]()
  }

  def <>(that: ConSingle[T]): ConSingle[T] = (this, that) match {
    case (ConMany(), _) => that
    case (_, ConMany()) => this
    case (ConOne(t), ConOne(u)) if t == u => this
    case _ => ConZero[T]()
  }
}
case class ConZero[T]() extends ConSingle[T]
case class ConOne[T](value: T) extends ConSingle[T]
case class ConMany[T]() extends ConSingle[T]

case class ConSingleUtil[T]() extends ConDomainUtil[T, ConSingle[T]] {
  type Dom = ConSingle[T]
  val Top = ConMany[T]()
  val Bot = ConZero[T]()
}

////////////////////////////////////////////////////////////////////////////////
// concrete finite set domain
////////////////////////////////////////////////////////////////////////////////
sealed abstract class ConSet[T] extends ConDomain[T, ConSet[T]] {
  def isBottom: Boolean = this == ConFin[T]()
  def isTop: Boolean = this == ConInf[T]()

  override def toString: String = this match {
    case ConFin(set) => "[" + set.mkString(", ") + "]"
    case ConInf() => "[< infinite values >]"
  }

  def <=(that: ConSet[T]): Boolean = (this, that) match {
    case (_, ConInf()) => true
    case (ConFin(lset), ConFin(rset)) => lset subsetOf rset
    case _ => false
  }

  def +(that: ConSet[T]): ConSet[T] = (this, that) match {
    case (ConInf(), _) | (_, ConInf()) => ConInf[T]()
    case (ConFin(lset), ConFin(rset)) => ConFin(lset ++ rset)
  }

  def <>(that: ConSet[T]): ConSet[T] = (this, that) match {
    case (ConInf(), _) => that
    case (_, ConInf()) => this
    case (ConFin(lset), ConFin(rset)) => ConFin(lset intersect rset)
  }
}
case class ConInf[T]() extends ConSet[T]
case class ConFin[T](values: Set[T]) extends ConSet[T]
object ConFin {
  def apply[T](seq: T*): ConFin[T] = ConFin(seq.toSet)
}

case class ConSetUtil[T]() extends ConDomainUtil[T, ConSet[T]] {
  type Dom = ConSet[T]
  val Top = ConInf[T]()
  val Bot = ConFin[T]()
}
