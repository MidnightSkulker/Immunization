package json

import spray.json._
import DefaultJsonProtocol._ // if you don't supply your own Protocol
import scala.io.Source._
import scala.io.BufferedSource
import models.model._

// Get information about doses from Json data
class JsonDoses (filename: String) {
  // Convert a buffered source of strings to a string
  def flattenSource(iter: BufferedSource): String = {
    var glob: String = ""
    for (line <- iter) { glob += line }
    return glob
  }
  // Read the Json information from a file.
  def readFile(filename: String): BufferedSource = fromFile(filename)
  // Process the Json information into a string.
  def stringify(file: BufferedSource): String = flattenSource(file)
  // Convert json string into JsValue.
  def jsonify(glob: String): spray.json.JsValue = glob.parseJson
  // Convert JsValue into a list of maps, one per student.
  def mapifyJson(json: spray.json.JsValue): JsonMaps = json.convertTo[JsonMaps]
  // Process all the way from the file to mapified Json.
  def mapify(filename: String): JsonMaps =
    stringify(readFile(filename)).parseJson.convertTo[JsonMaps]
  // ***** Need to convert value to DateTime
}

object JsonDoses {}
