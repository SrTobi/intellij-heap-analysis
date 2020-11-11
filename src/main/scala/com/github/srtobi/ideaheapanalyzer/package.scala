package com.github.srtobi

import org.netbeans.lib.profiler.heap.{ArrayItemValue, FieldValue, Instance, ObjectFieldValue, Value}

package object ideaheapanalyzer {
  implicit class ValueOps(private val field: Value) extends AnyVal {
    def outgoungReference(isSpecial: FieldValue => Boolean = _ => false): Option[Instance] = field match {
      case item: ArrayItemValue => Option(item.getInstance())
      case objField: ObjectFieldValue if !isSpecial(objField) => Option(objField.getInstance())
      case _ => None
    }
  }
}
