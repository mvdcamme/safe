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

package kr.ac.kaist.safe.analyzer.console.command

import jline.console.ConsoleReader
import kr.ac.kaist.safe.analyzer.console._
import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.analyzer.domain.Utils._
import kr.ac.kaist.safe.LINE_SEP
import kr.ac.kaist.safe.nodes.cfg.{ CFGCallInst, CFGNormalInst }

// run instructions
case object CmdRunInsts extends Command("run_insts", "Run instruction by instruction.") {
  def help: Unit = println("usage: " + name)
  def run(c: Console, args: List[String]): Option[Target] = {
    args match {
      case Nil => {
        val cp = c.getCurCP
        val st = c.sem.getState(cp)
        val block = cp.block
        val insts = block.getInsts.reverse
        val reader = new ConsoleReader()
        insts match {
          case Nil => println("* no instructions")
          case _ => println(c.getCurCP.block.toString(0))
        }
        val (resSt, resExcSt, _) = insts.foldLeft((st, AbsState.Bot, true)) {
          case ((oldSt, oldExcSt, true), inst) =>
            println
            reader.setPrompt(
              s"inst: [${inst.id}] $inst" + LINE_SEP +
                s"('s': state / 'q': stop / 'n','': next)" + LINE_SEP +
                s"> "
            )
            var line = ""
            while ({
              line = reader.readLine
              line match {
                case "s" => {
                  println("*** state ***")
                  println(oldSt.toString)
                  println
                  println("*** exception state ***")
                  println(oldExcSt.toString)
                  true
                }
                case "d" => true // TODO diff
                case "n" | "" => false
                case "q" => false
                case _ => true
              }
            }) {}
            line match {
              case "q" => (oldSt, oldExcSt, false)
              case _ =>
                val (st, excSt) = inst match {
                  case i: CFGNormalInst => c.sem.I(i, oldSt, oldExcSt)
                  case i: CFGCallInst => c.sem.CI(cp, i, oldSt, oldExcSt)
                }
                (st, excSt, true)
            }
          case (old @ (_, _, false), inst) => old
        }
        println("*** state ***")
        println(resSt.toString)
        println
        println("*** exception state ***")
        println(resExcSt.toString)
      }
      case _ => help
    }
    None
  }
}
