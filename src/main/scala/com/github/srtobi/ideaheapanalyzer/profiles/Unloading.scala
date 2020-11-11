package com.github.srtobi.ideaheapanalyzer
package profiles

import org.netbeans.lib.profiler.heap.{Heap, Instance}

import scala.jdk.CollectionConverters._


object Unloading extends AnalyzerProfileBase {
  override def shouldBeStartOfPath(instance: Instance): Boolean =
    instance.getJavaClass.getName.contains(".scala")

  override def filterPathFromAfterStartToGcRoot(path: Seq[Instance]): Boolean =
    !path.exists(_.getJavaClass.getName.contains(".scala"))

  override def ignoreInstance(inst: Instance): Boolean =
    inst.getJavaClass.getName.contains("cl.PluginClassLoader")

  override def getRoots(heap: Heap): IterableOnce[Instance] =
    heap.getGCRoots.asScala
      .map(_.getInstance)
}
