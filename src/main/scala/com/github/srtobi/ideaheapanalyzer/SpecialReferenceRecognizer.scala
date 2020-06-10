package com.github.srtobi.ideaheapanalyzer

import org.netbeans.lib.profiler.heap.{Field, FieldValue, Heap, Instance}

import scala.jdk.CollectionConverters._

class SpecialReferenceRecognizer(heap: Heap) {
  private val REF_CLASSES = Array(
    "java.lang.ref.WeakReference", // NOI18N
    "java.lang.ref.SoftReference",
    "java.lang.ref.FinalReference",
    "java.lang.ref.PhantomReference"
  )
  private val JAVA_LANG_REF_REFERENCE = "java.lang.ref.Reference"
  private val REFERENT_FILED_NAME = "referent"

  private lazy val referentField = computeReferentField()
  private lazy val referenceClasses =
    REF_CLASSES.flatMap { refClass =>
      val ref = heap.getJavaClassByName(refClass)
      if (ref != null) {
        ref +: ref.getSubClasses.asScala.toSeq
      } else {
        Seq.empty
      }
    }.toSet


  def isSpecialReference(value: FieldValue, instance: Instance): Boolean = {
    val f = value.getField
    f == referentField && referenceClasses.contains(instance.getJavaClass)
  }


  private def computeReferentField(): Field = {
    val reference = heap.getJavaClassByName(JAVA_LANG_REF_REFERENCE)
    val fieldRef = reference.getFields.iterator
    while (fieldRef.hasNext) {
      val f: Field = fieldRef.next
      if (f.getName == REFERENT_FILED_NAME)
        return f
    }
    throw new IllegalArgumentException("reference field not found in " + reference.getName) // NOI18N
  }
}
