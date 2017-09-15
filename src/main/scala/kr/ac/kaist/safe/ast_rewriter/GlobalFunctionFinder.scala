/**
  * *****************************************************************************
  * Copyright (c) 2016, KAIST.
  * All rights reserved.
  *
  * Use is subject to license terms.
  *
  * This distribution may include materials developed by third parties.
  * ****************************************************************************
  */

package kr.ac.kaist.safe.ast_rewriter

import kr.ac.kaist.safe.nodes.ast._

class GlobalFunctionFinder(program: Program) extends ASTWalker {

  lazy val result: Program = doit

  private def doit: Program = walk(program)

  private var isGlobal = true

  override def walk(node: Functional) = node match {
    case Functional(info, fds, vds, stmts, name, params, body, _) =>
      val oldIsGlobal = isGlobal
      isGlobal = false
      val temp = Functional(walk(info), fds.map(walk), vds.map(walk), walk(stmts), walk(name),
        params.map(walk), body, oldIsGlobal)
      isGlobal = oldIsGlobal
      temp
  }

}
