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

package kr.ac.kaist.safe.analyzer.Hybrid

import kr.ac.kaist.safe.analyzer._
import kr.ac.kaist.safe.analyzer.domain.AbsHeap
import kr.ac.kaist.safe.analyzer.domain.AbsObject
import kr.ac.kaist.safe.analyzer.domain.AbsState
import kr.ac.kaist.safe.analyzer.domain.Utils._
import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.analyzer.domain.DefaultState._
import kr.ac.kaist.safe.analyzer.models.builtin.BuiltinGlobal
import kr.ac.kaist.safe.interpreter._
import kr.ac.kaist.safe.interpreter.objects._
import kr.ac.kaist.safe.nodes.cfg._
import kr.ac.kaist.safe.nodes.ir._
import kr.ac.kaist.safe.util._
import spray.json.JsObject

object DefaultStateConverter {

  private def computeInitialHeap(cfg: CFG, env: Env): AbsHeap = {
    val initState = Initialize(cfg, false)
    addISEnv(initState, env).heap
  }

  private def convertValError(valError: ValError, heap: AbsHeap): Value = valError match {
    case PVal(v) =>
      v.value match {
        case EJSNumber(_, num) => Num(num)
        case EJSString(str) => Str(str)
        case EJSBool(bool) => Bool(bool)
        case EJSUndef => Undef
        case EJSNull => Null
      }
    case error: JSError =>
      ???
    case obj: JSObject =>
      val convertedObject = convertJSObject(obj, heap)
      ???
  }

  private def convertJSObject(jsObject: JSObject, heap: AbsHeap): Object = {
    //    val (absObject, exceptions) = obj.Put(AbsString(propertyName), convertedValue, true, heap)
    //    propertyValue
    //    if (exceptions.nonEmpty) {
    //      println(s"Got exceptions $exceptions")
    //      throw new RuntimeException
    //    } else {
    //      absObject
    //    }
    val amap: Map[String, DataProp] = jsObject.property.map.foldLeft[Map[String, DataProp]](Map())({
      case (amap, (propertyName, propertyValue)) => propertyValue.value match {
        case Some(value) =>
          val convertedValue = convertValError(value, heap)
          val dataProp = DataProp(
            convertedValue,
            propertyValue.isWritable,
            propertyValue.isEnumerable,
            propertyValue.isConfigurable
          )
          amap + (propertyName -> dataProp)
        case None =>
          println(s"Don't yet know what to do here") // TODO MV
          throw new RuntimeException
      }
    })
    val imap: Map[IName, IValue] = {
      val className: Value = Str(jsObject.className)
      val extensible: Value = Bool(jsObject.extensible)
      val prototype: Value = ??? // convertJSObject(jsObject.proto, heap)
      ???
    }
    Object(amap, imap)
  }

  private def addISEnv(state: AbsState, env: Env): AbsState = env match {
    case EmptyEnv() =>
      state
    case ConsEnv(first, rest) => first match {
      case DeclEnvRec(store) =>
        val newState = store.foldLeft(state)({
          case (state, (variable, storeValue)) =>
            val convertedStoreValue = convertValError(storeValue.value, state.heap)
            //            state.createMutableBinding(CFGTempId(variable, PureLocalVar), convertedStoreValue)
            ???
        })
        addISEnv(newState, rest)
      case ObjEnvRec(obj) =>
        val convertedValue = convertJSObject(obj, state.heap)
        state.heap
        ???
    }
  }

  def convert(cfg: CFG, IS: InterpreterState, cp: ControlPoint): AbsState = {
    val initialHeap = computeInitialHeap(cfg, IS.env)
    ???
  }

}
