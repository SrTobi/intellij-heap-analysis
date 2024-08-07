package com.github.srtobi

import org.netbeans.lib.profiler.heap.*
import scala.jdk.CollectionConverters.*

package object ideaheapanalyzer {
  implicit class ValueOps(private val field: Value) extends AnyVal {
    def outgoungReference(isSpecial: FieldValue => Boolean = _ => false): Option[Instance] = field match {
      case item: ArrayItemValue => Option(item.getInstance())
      case objField: ObjectFieldValue if !isSpecial(objField) => Option(objField.getInstance())
      case _ => None
    }
  }

  enum Item {
    case ArrayItem(index: Int)
    case FieldItem(field: Field)
    case UnknownItem
  }
  extension (instance: Instance) {
    def outgoingReferences(isSpecial: (FieldValue, Instance) => Boolean = (_, _) => false): Iterator[Instance] = instance match {
      case array: ObjectArrayInstance => array.getValues.iterator().asScala.filterNot(_ == null)
      case inst: Instance =>
        (inst.getFieldValues.iterator().asScala ++
          inst.getStaticFieldValues.iterator().asScala)
          .flatMap(_.outgoungReference(isSpecial(_, instance))) /*++
            Iterator(inst.getJavaClass.getClassLoader)*/
    }

    def fieldPointingTo(target: Instance): Item = instance match {
      case array: ObjectArrayInstance =>
        val index = array.getValues.asScala.indexOf(target)
        if (index >= 0) Item.ArrayItem(index)
        else Item.UnknownItem
      case inst: Instance =>
        (inst.getFieldValues.iterator().asScala ++
          inst.getStaticFieldValues.iterator().asScala)
          .find(_.outgoungReference().contains(target))
          .map(_.getField)
          .map(Item.FieldItem(_))
          .getOrElse(Item.UnknownItem)
    }

    def pathToGc: Seq[Instance] =
      Iterator.unfold(instance) {
        cur =>
          assert(cur.getNearestGCRootPointer != null)
          Option(cur.getNearestGCRootPointer).filter(_ != cur).map(x => x -> x)
      }.toSeq
  }
}
