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

package kr.ac.kaist.safe.analyzer.models

import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.analyzer.domain.Utils.AbsValue
import kr.ac.kaist.safe.nodes.cfg.CFG

object SelfModel extends Model {
  def init(h: AbsHeap, cfg: CFG): (AbsHeap, AbsValue) = (h, AbsValue.Bot)
}
