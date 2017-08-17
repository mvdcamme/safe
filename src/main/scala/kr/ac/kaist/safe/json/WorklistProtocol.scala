/**
 * *****************************************************************************
 * Copyright (c) 2017, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.json

import kr.ac.kaist.safe.nodes.cfg._
import kr.ac.kaist.safe.analyzer._
import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.json.AbsStateProtocol._
import kr.ac.kaist.safe.json.AbsLexEnvProtocol._
import kr.ac.kaist.safe.json.AbsValueProtocol._
import kr.ac.kaist.safe.errors.error.{
  WorklistParseError,
  ControlPointParseError,
  TracePartitionParseError,
  LoopInfoParseError,
  BlockNotFoundError,
  EdgeDataParseError,
  SemanticsParseError
}

import spray.json._
import DefaultJsonProtocol._

object WorklistProtocol extends DefaultJsonProtocol {

  var cfg: CFG = _
  var worklist: Worklist = _

  implicit object TracePartitionJsonFormat extends RootJsonFormat[TracePartition] {

    def write(tp: TracePartition): JsValue = tp match {
      case EmptyTP => JsNull
      case ProductTP(ltp, rtp) => JsArray(
        JsString("p"),
        ltp.toJson,
        rtp.toJson
      )
      case CallSiteContext(callsites, depth) => JsArray(
        JsString("c"),
        JsArray(callsites.map(call => JsArray(
          JsNumber(call.func.id),
          JsNumber(call.id)
        )).to[Vector]),
        JsNumber(depth)
      )
      case LoopContext(infoOpt, outerOpt, depth) => JsArray(
        JsString("l"),
        infoOpt match {
          case Some(info) => loopInfoToJson(info)
          case None => JsNull
        },
        outerOpt match {
          case Some((context, block)) => JsArray(
            context.asInstanceOf[TracePartition].toJson,
            JsNumber(block.func.id),
            JsNumber(block.id)
          )
          case None => JsNull
        },
        JsNumber(depth)
      )
    }

    def read(value: JsValue): TracePartition = value match {
      case JsNull => EmptyTP
      case JsArray(Vector(JsString("p"), ltp, rtp)) =>
        ProductTP(ltp.convertTo[TracePartition], rtp.convertTo[TracePartition])
      case JsArray(Vector(JsString("c"), JsArray(callsites), JsNumber(depth))) =>
        CallSiteContext(callsites.to[List].map(_ match {
          case JsArray(Vector(JsNumber(fid), JsNumber(bid))) =>
            cfg.getBlock(fid.toInt, bid.toInt) match {
              case Some(block) => block.asInstanceOf[Call]
              case None => throw BlockNotFoundError("TracePartition", fid.toInt, bid.toInt)
            }
          case _ => throw TracePartitionParseError(value)
        }), depth.toInt)
      case JsArray(Vector(JsString("l"), info, outer, JsNumber(depth))) => LoopContext(
        info match {
          case JsNull => None
          case _ => Some(jsonToLoopInfo(info))
        },
        outer match {
          case JsNull => None
          case JsArray(Vector(context, JsNumber(fid), JsNumber(bid))) => Some((
            context.convertTo[TracePartition].asInstanceOf[LoopContext],
            cfg.getBlock(fid.toInt, bid.toInt) match {
              case Some(block) => block.asInstanceOf[NormalBlock]
              case None => throw BlockNotFoundError("TracePartition", fid.toInt, bid.toInt)
            }
          ))
          case _ => throw TracePartitionParseError(value)
        },
        depth.toInt
      )
      case _ => throw TracePartitionParseError(value)
    }
  }

  private def loopInfoToJson(info: LoopInfo): JsValue = info match {
    case LoopInfo(head, k, outer) => JsArray(
      JsNumber(head.func.id),
      JsNumber(head.id),
      JsNumber(k),
      outer.asInstanceOf[TracePartition].toJson
    )
  }

  private def jsonToLoopInfo(value: JsValue): LoopInfo = value match {
    case JsArray(Vector(JsNumber(fid), JsNumber(bid), JsNumber(k), outer)) =>
      LoopInfo(
        cfg.getBlock(fid.toInt, bid.toInt) match {
          case Some(block) => block.asInstanceOf[LoopHead]
          case None => throw BlockNotFoundError("LoopInfo", fid.toInt, bid.toInt)
        },
        k.toInt,
        outer.convertTo[TracePartition].asInstanceOf[LoopContext]
      )
    case _ => throw LoopInfoParseError(value)
  }

  implicit object ControlPointJsonFormat extends RootJsonFormat[ControlPoint] {

    def write(cp: ControlPoint): JsValue = cp match {
      case ControlPoint(block, tp) => JsArray(
        JsNumber(block.func.id),
        JsNumber(block.id),
        tp.toJson
      )
    }

    def read(value: JsValue): ControlPoint = value match {
      case JsArray(Vector(JsNumber(fid), JsNumber(bid), tp)) =>
        cfg.getBlock(fid.toInt, bid.toInt) match {
          case Some(block) =>
            ControlPoint(block, tp.convertTo[TracePartition])
          case None =>
            throw BlockNotFoundError("ControlPoint", fid.toInt, bid.toInt)
        }
      case _ => throw ControlPointParseError(value)
    }
  }

  implicit object WorklistJsonFormat extends RootJsonFormat[Worklist] {

    def write(worklist: Worklist): JsValue =
      JsArray(worklist.getWorklist.reverse.map(_ match {
        case worklist.Work(order, cp) => cp.toJson
      }).to[Vector])

    def read(value: JsValue): Worklist = value match {
      case JsArray(works) => {
        val worklist = Worklist(cfg)
        for (work <- works)
          worklist.add(work.convertTo[ControlPoint])
        worklist
      }
      case _ => throw WorklistParseError(value)
    }
  }

  implicit object EdgeDataJsonFormat extends RootJsonFormat[EdgeData] {

    def write(data: EdgeData): JsValue = data match {
      case EdgeData(old, env, binding) => JsArray(
        old.toJson, env.toJson, binding.toJson
      )
    }

    def read(value: JsValue): EdgeData = value match {
      case JsArray(Vector(old, env, binding)) => EdgeData(
        old.convertTo[OldASiteSet],
        env.convertTo[AbsLexEnv],
        binding.convertTo[AbsValue]
      )
      case _ => throw EdgeDataParseError(value)
    }
  }

  implicit object SemanticsFormat extends RootJsonFormat[Semantics] {

    def write(sem: Semantics): JsValue = JsArray(
      JsArray(sem.getAllState.foldLeft[Vector[JsValue]](Vector()) {
        case (vec, (block, map)) => vec ++ (map.toSeq map {
          case (tp, state) => JsArray(Vector(
            ControlPoint(block, tp).toJson,
            state.toJson
          ))
        })
      }),
      JsArray(sem.getAllIPSucc.to[Vector] map {
        case (cp1, map) => JsArray(Vector(
          cp1.toJson,
          JsArray(map.to[Vector] map {
            case (cp2, edge) => JsArray(Vector(
              cp2.toJson,
              edge.toJson
            ))
          })
        ))
      })
    )

    def read(value: JsValue): Semantics = value match {
      case JsArray(Vector(JsArray(state), JsArray(succ))) => {
        val sem = new Semantics(cfg, worklist)
        for (cpAndSt <- state)
          cpAndSt match {
            case JsArray(Vector(cp, st)) => sem.setState(
              cp.convertTo[ControlPoint], st.convertTo[AbsState]
            )
            case _ => throw SemanticsParseError(value)
          }
        sem.setAllIPSucc(succ.map(_ match {
          case JsArray(Vector(cp1, JsArray(map))) => (
            cp1.convertTo[ControlPoint],
            map.map(_ match {
              case JsArray(Vector(cp2, edge)) => (
                cp2.convertTo[ControlPoint],
                edge.convertTo[EdgeData]
              )
              case _ => throw SemanticsParseError(value)
            }).toMap
          )
          case _ => throw SemanticsParseError(value)
        }).toMap)
        sem
      }
      case _ => throw SemanticsParseError(value)
    }
  }
}
