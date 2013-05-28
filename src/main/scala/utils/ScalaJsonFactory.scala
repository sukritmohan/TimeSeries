package utils

import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.databind.{DeserializationFeature, SerializationFeature, ObjectMapper}
import java.io.File

/**
 * @author karthik@quantifind.com, sukrit
 * Date: 1/3/12
 *
 */

object ScalaJsonFactory extends java.io.Serializable {

  private var _objectMapper = getObjectMapperInternal()

  def getObjectMapper() = {

    if (_objectMapper == null)
      _objectMapper = getObjectMapperInternal()

    _objectMapper
  }


  private def getObjectMapperInternal(): ObjectMapper = {

    val mapper = new ObjectMapper()

    // Don't write null map values
    mapper.configure(SerializationFeature.WRITE_NULL_MAP_VALUES, false)

    // Don't fail on serialization when there are null fields in the class
    mapper.configure(SerializationFeature.FAIL_ON_EMPTY_BEANS, false)

    // When there are unknown properties in the JSON (some unused fields), don't fail
    mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)

    // Scala specific. Register the scala module with the asl mapper
    mapper.registerModule(DefaultScalaModule)

    mapper
  }

  def saveJsonToFile(file: String, obj: Any) = {
    getObjectMapper().writeValue(new File(file), obj)
  }

  def getJsonFromObj(obj: Any): String = {
    getObjectMapper().writeValueAsString(obj)
  }

  def showReadToObjFormat() = {
    println("""ScalaJsonFactory.getObjectMapper.readValue(json, classOf[T])""")
  }

}
