package com.github.srtobi.ideaheapanalyzer
package profiles

import org.netbeans.lib.profiler.heap.{Heap, Instance}

import scala.jdk.CollectionConverters._


object ProjectLeak extends AnalyzerProfileBase {
  override def shouldBeStartOfPath(instance: Instance): Boolean =
    instance.getJavaClass.getName.contains("ScalaRenameClassTest")

  override def filterPathFromAfterStartToGcRoot(path: Seq[Instance]): Boolean =
  //!path.init // <- last is gc root which is definitely scala, becauso of getRoots
  //  .exists(inst => inst.getJavaClass.getName.contains(".scala"))
    true

  override def ignoreInstance(inst: Instance): Boolean = false
    //inst.isGCRoot ||
    //  (inst.getJavaClass.getName.contains(".scala") &&
    //    inst.outgoingReferences().exists(_.getJavaClass.getName.endsWith("ProjectExImpl"))
    //    ) ||
    //  inst.getJavaClass.getName.contains("PsiManagerImpl")
  //inst.getJavaClass.getName.contains("cl.PluginClassLoader")

  override def getRoots(heap: Heap): IterableOnce[Instance] =
    heap.getGCRoots.asScala
      .map(_.getInstance())

  /*override def shouldBeStartOfPath(instance: Instance): Boolean =
    instance.getJavaClass.getName.matches(".*ProjectExImpl")

  override def filterPathFromAfterStartToGcRoot(path: Seq[Instance]): Boolean =
    //!path.init // <- last is gc root which is definitely scala, becauso of getRoots
    //  .exists(inst => inst.getJavaClass.getName.contains(".scala"))
    true

  override def ignoreInstance(inst: Instance): Boolean =
    inst.isGCRoot ||
      (inst.getJavaClass.getName.contains(".scala") &&
        inst.outgoingReferences().exists(_.getJavaClass.getName.endsWith("ProjectExImpl"))
      ) ||
      inst.getJavaClass.getName.contains("PsiManagerImpl")
  //inst.getJavaClass.getName.contains("cl.PluginClassLoader")

  override def getRoots(heap: Heap): IterableOnce[Instance] =
    heap.getAllInstances.asScala
      .iterator
      .filter(_.getJavaClass.getName.contains(".scala"))*/
}
