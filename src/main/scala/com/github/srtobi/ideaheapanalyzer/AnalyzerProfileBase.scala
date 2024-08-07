package com.github.srtobi.ideaheapanalyzer

import java.io.File
import java.nio.file.{Path, Paths}
import org.netbeans.lib.profiler.heap.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

abstract class AnalyzerProfileBase {
  /////////////////////////////////////////////////////////////////////////////////////
  ////////////////// Adjust these functions to suit your needs ////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////
  def shouldBeStartOfPath(instance: Instance): Boolean
  def filterPathFromAfterStartToGcRoot(path: Seq[Instance]): Boolean
  def ignoreInstance(inst: Instance): Boolean
  def getRoots(heap: Heap): IterableOnce[Instance]

  /////////////////////////////////////////////////////////////////////////////////////
  ///////////////////////////////// And run ///////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////
  def main(args: Array[String]): Unit = {
    val heap: Heap = HeapFactory.createHeap(findHProf())
    val isSpecialReference = new SpecialReferenceRecognizer(heap).isSpecialReference
    val reachables = mutable.Set.empty[Instance]
    val unprocessed = mutable.Queue.empty[Instance]
    val parentMap = mutable.Map.empty[Instance, Instance]

    def addToUnprocessed(inst: Instance, from: Instance = null): Unit = {
      if (ignoreInstance(inst))
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
    getRoots(heap)
      .iterator
      .foreach(addToUnprocessed(_))


    while (unprocessed.nonEmpty) {
      val cur = unprocessed.dequeue()
      cur
        .outgoingReferences(isSpecialReference)
        .foreach(addToUnprocessed(_, cur))
    }

    @tailrec
    def pathToGcRoot(inst: Instance, acc: List[Instance] = Nil): List[Instance] = {
      parentMap.get(inst) match {
        case Some(parent) => pathToGcRoot(parent, parent :: acc)
        case None => acc.reverse
      }
    }

    val groupedTailLength = 5
    val grouped = mutable.Buffer.empty[(List[(Item, Instance)], List[String])]

    println("Result:")
    var n = 0
    reachables
      .iterator
      .filter(shouldBeStartOfPath)
      //.filterNot(reachables.contains)
      .foreach { inst =>
        val path = pathToGcRoot(inst)
        if (filterPathFromAfterStartToGcRoot(path)) {
          val fullPath = inst :: path
          val fieldInInst = makeFieldPointingToMap(fullPath)
          n += 1
          val (skipRange, skipped) = if (fullPath.size > 100) {
            (Range(20, fullPath.size - 20), fullPath.size - 40)
          } else (Range(-1, -1), -1)
          val builder = new StringBuilder
          var indent = 0
          for {
            (inst, i) <- fullPath.zipWithIndex
            if !skipRange.contains(i)
          } {
            builder ++= "  " * indent
            indent += 1
            for (item <- fieldInInst.get(inst)) {
              builder ++= itemToString(item)
              builder ++= " in "
            }
            builder ++= inst.getJavaClass.getName + "#" + inst.getInstanceNumber
            builder += '\n'
            if (i == skipRange.start - 1) {
              builder ++= "  " * indent
              builder ++= s"<skipped $skipped entries>\n"
              indent += 1
            }
          }
          val str = builder.result()
          println(str)

          // Try to group paths together to have a more comprehensive list
          val list = path.map(inst => fieldInInst(inst) -> inst)
          val idx = grouped.indexWhere(tup => pathEquals(tup._1, list))
          if (idx < 0) {
            grouped += list -> List(str)
          } else {
            val (list, strs) = grouped(idx)
            grouped(idx) = list -> (str :: strs)
          }
        }
      }

    println("\n" * 3)
    println("Count: " + n)

    println()
    println("==================================================================================================================")
    println(s"==================================================== Grouped (${grouped.size}) ================================================")
    println("==================================================================================================================")
    println()

    val indent = "#" + " " * 16
    grouped.map(_._2).foreach {
      case head :: rest =>
        println(head)
        //for (r <- rest) {
        //  println(indent + r.replace("\n", "\n" + indent))
        //  println()
        //}
    }
  }

  def findHProf(): File = {
    def findIn(path: String): Seq[File] =
      Paths.get(path).toFile
        .listFiles()
        .filter(_.getPath.endsWith(".hprof"))
        .sortBy(_.getName)

    val profileFiles =
      findIn(System.getProperty("user.home")) ++
        findIn(".")

    println("Found head dumps:")
    profileFiles.foreach(println)
    profileFiles.lastOption.getOrElse(throw new Exception("Couldn't find any dumps in home directory"))
  }

  def makeFieldPointingToMap(path: List[Instance]): Map[Instance, Item] =
    path.zip(path.tail).map {
      case (to, from) => from -> from.fieldPointingTo(to)
    }.toMap

  def itemToString(item: Item, shadowIndex: Boolean = false): String = item match {
    case Item.ArrayItem(_) if shadowIndex => "[_]"
    case Item.ArrayItem(index) => s"[$index]"
    case Item.FieldItem(field) => s"${field.getName}"
    case Item.UnknownItem => "unknown"
  }

  def pathEquals(a: List[(Item, Instance)], b: List[(Item, Instance)]): Boolean = {
    val pre = 3
    def itemInInstanceToComparable(tup: (Item, Instance)): Equals =
      (itemToString(tup._1, shadowIndex = true), tup._2.getJavaClass)

    val ax = a.map(itemInInstanceToComparable)
    val bx = b.map(itemInInstanceToComparable)
    val aMap = ax.take(pre).toSet

    // find first that is equal and check if rest is somewhat equal
    val idx = bx.indexWhere(aMap.contains)
    if (0 <= idx && idx < pre) {
      val first = bx(idx)
      val aRest = ax.dropWhile(_ != first).toSet
      val bRest = bx.dropWhile(_ != first).toSet
      val intersection = (aRest intersect bRest).size.toFloat
      (intersection / math.min(aRest.size, bRest.size)) > 0.5
    } else false
  }
}