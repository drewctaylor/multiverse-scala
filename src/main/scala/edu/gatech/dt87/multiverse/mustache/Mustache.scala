package edu.gatech.dt87.multiverse.mustache

import java.io.StringReader
import java.io.StringWriter

import com.github.mustachejava.DefaultMustacheFactory
import collection.JavaConverters.mapAsJavaMap

object Mustache {

  def apply(template: String): Map[String, Map[String, Any]] => String = {

    scope => {

      val stringWriter = new StringWriter()

      new DefaultMustacheFactory().compile(new StringReader(template), "").execute(stringWriter, mapAsJavaMap(scope.mapValues(mapAsJavaMap(_))))
      stringWriter.flush()

      stringWriter.toString
    }
  }
}
