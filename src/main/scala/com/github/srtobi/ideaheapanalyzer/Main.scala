package com.github.srtobi.ideaheapanalyzer

import java.io.File
import java.nio.file.Paths

import org.netbeans.lib.profiler.heap._

import scala.collection.mutable
import scala.jdk.CollectionConverters._

object Main {
  sealed trait Item
  case class ArrayItem(index: Int) extends Item
  case class FieldItem(field: Field) extends Item
  case object UnknownItem extends Item

  implicit class InstanceOps(private val instance: Instance) {
    assert(instance != null)

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
        if (index >= 0) ArrayItem(index)
        else UnknownItem
      case inst: Instance =>
        (inst.getFieldValues.iterator().asScala ++
          inst.getStaticFieldValues.iterator().asScala)
          .find(_.outgoungReference().contains(target))
          .map(_.getField)
          .map(FieldItem)
          .getOrElse(UnknownItem)
    }

    def pathToGc: Seq[Instance] =
      Iterator.unfold(instance) {
        cur =>
          assert(cur.getNearestGCRootPointer != null)
          Option(cur.getNearestGCRootPointer).filter(_ != cur).map(x => x -> x)
      }.toSeq

  }

  implicit class ValueOps(private val field: Value) extends AnyVal {
    def outgoungReference(isSpecial: FieldValue => Boolean = _ => false): Option[Instance] = field match {
      case item: ArrayItemValue => Option(item.getInstance())
      case objField: ObjectFieldValue if !isSpecial(objField) => Option(objField.getInstance())
      case _ => None
    }
  }

  def findHProf(): File = {
    val profileFiles = Paths.get(System.getProperty("user.home")).toFile
      .listFiles()
      .filter(_.getPath.endsWith(".hprof"))
      .sortBy(_.getName)

    println("Found head dumps:")
    profileFiles.foreach(println)
    profileFiles.lastOption.getOrElse(throw new Exception("Couldn't find any dumps in home directory"))
  }

  def main(args: Array[String]): Unit = {
    val heap: Heap = HeapFactory.createHeap(findHProf())
    val isSpecialReference = new SpecialReferenceRecognizer(heap).isSpecialReference _
    val reachables = mutable.Set.empty[Instance]
    val unprocessed = mutable.Queue.empty[Instance]
    val parentMap = mutable.Map.empty[Instance, Instance]

    def addToUnprocessed(inst: Instance, from: Instance = null): Unit = {
      if (inst.getJavaClass.getName.contains("cl.PluginClassLoader"))
        return

      if (reachables.add(inst)) {
        unprocessed += inst
        Option(from).foreach(parentMap += inst -> _)
      }
    }

    println("gc roots: " + heap.getGCRoots.size())
    heap.getGCRoots.asScala
      .groupBy(_.getKind)
      .foreach {
        case (kind, insts) => println(kind + ": " + insts.size)
      }

    println(s"Analyse dump... (${heap.getSummary.getTotalLiveInstances} instances)")
    heap.getGCRoots.asScala
      .map(_.getInstance)
      .foreach(addToUnprocessed(_))


    while (unprocessed.nonEmpty) {
      val cur = unprocessed.dequeue()
      cur
        .outgoingReferences(isSpecialReference)
        .foreach(addToUnprocessed(_, cur))
    }

    def pathToGcRoot(inst: Instance): List[Instance] = {
      parentMap.get(inst) match {
        case Some(parent) => parent :: pathToGcRoot(parent)
        case None => Nil
      }
    }

    println("Unreachable instances:")
    var n = 0
    reachables
      .iterator
      .filter(_.getJavaClass.getName.contains(".scala"))
      //.filterNot(reachables.contains)
      .foreach { inst =>
        val path = pathToGcRoot(inst)
        if (!path.exists(_.getJavaClass.getName.contains(".scala"))) {
          n += 1
          var prev = Option.empty[Instance]
          for ((inst, i) <- (inst::path).zipWithIndex) {
            print("  " * i)
            for (item <- prev.map(inst.fieldPointingTo)) item match {
              case ArrayItem(index) => print(s"[$index] in ")
              case FieldItem(field) => print(s"${field.getName} in ")
              case UnknownItem => print("unknown in ")
            }
            println(inst.getJavaClass.getName + "#" + inst.getInstanceNumber)
            prev = Some(inst)
          }
        }
      }

    println("Count: " + n)
  }
}
