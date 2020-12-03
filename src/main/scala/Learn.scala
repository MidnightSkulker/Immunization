// Can only declare a package for scalac compilation.
// This is ignored in the REPL, it just generates a warning.
// package com.twitter.example
// package scala.language.implicitConversions
// import cats.effect.IO
import spray.json._
import DefaultJsonProtocol._ // if you don't supply your own Protocol (see below)
import scala.io.Source._
import org.joda.time.base.AbstractInstant
import org.joda.time.DateTime
import org.joda.time.Period

object variableArgument {
  def printAll(strings: String*) : Unit = {
    for( value <- strings ) { println(value); }
  }
  def capitalizeAll(args: String*) = { args.map { arg => arg.capitalize } }
}

class Calkulator(brand: String) {
  val color: String =
    if (brand == "TI") { "blue" }
    else if (brand == "HP") { "black" }
    else { "white" }

  def add(m: Int, n: Int): Int = m + n
}

// Class inheritance.
class ScientificCalkulator(brand: String) extends Calkulator(brand) {
  def log(m: Double, base: Double): Double = math.log(m) / math.log(base)
}

// Overloading.
class EvenMoreScientificCalkulator(brand: String) extends ScientificCalkulator(brand) {
  def log(m: Int): Double = log(m, math.exp(1))
}

// Abstract Classes.
abstract class Shape {
  def getArea(): Int    // subclass should define this
}

// Inheriting an abstract class.
class Circle(r: Int) extends Shape {
  def getArea(): Int = { r * r * 3 }
}

// Traits are collections of fields and behaviors that you can
// extend or mixin to your classes.
trait Car {
  val brand: String
}

trait Shiny {
  val shineRefraction: Int
}

// Extending a trait.
class BMW extends Car {
  val brand = "BMW"
}

// Extending several traits.
class BMW2 extends Car with Shiny {
  val brand = "BMW"
  val shineRefraction = 12
}

// A trait used to define an abstract polymorphic type.
trait Cache[K, V] {
  def init(): Unit
  def get(key: K): V
  def put(key: K, value: V): Unit
  def delete(key: K): Unit
  def remove[K](key: K): Unit // Method that introduces a type parameter.
}

// Apply methods.
class Foo {}

// Single purpose class.
// Enables you to write this:
//    val newFoo = FooMaker ()
object FooMaker { def apply() = new Foo }
// Class Bar allows you to write these:
//   val bar = new Bar
//   bar()
class Bar { def apply() = 0 }

// Objects are a single instance of a class.
object Timer {
  var count: Int = 0
  def currentCount(): Int = { count += 1; count }
}

// You can have an objects as the same name as a class.
// This is used only for factories
class Bar2 (foo: String)
object Bar2 { def apply(foo: String) = new Bar2(foo) }

// Functions are objects (mayge ugh)
object addThree extends Function1[Int, Int] { def apply(m: Int): Int = m + 3 }
// The syntactic sugar of apply helps unify the duality of object and functional
// programming. You can pass classes around and use them as functions and
// functions are just instances of classes under the covers.
class AddFour extends Function1[Int, Int] { def apply(m: Int): Int = m + 4 }

// You can extend a function using this syntax.
class AddFive extends (Int => Int) { def apply(m: Int): Int = m + 5 }

// Case Classes.
case class Calculator(brand: String, model: String)

trait VaccineStatus extends Enumeration () {
  type Status = Value
  val Incomplete, UpToDate, Complete, None = Value
}

trait AgeRange {
  def withinRange(d: DateTime, dob: DateTime, startMonth: Int, endMonth: Int): Boolean =
    (d.isAfter(dob.plusMonths(startMonth)) && (d.isBefore(dob.plusMonths(endMonth))))
  def withinRange(d: Option[DateTime], dob: DateTime, startMonth: Int, endMonth: Int): Boolean =
    d match {
      case None => false
      case Some(d) => (d.isAfter(dob.plusMonths(startMonth)) && (d.isBefore(dob.plusMonths(endMonth))))
    }
}

trait Older {
  def olderThan(numberOfMonths: Int, dob: DateTime): Boolean =
    dob.plusMonths(numberOfMonths).isBefore(DateTime.now())
}

trait Younger {
  def youngerThan(numberOfMonths: Int, dob: DateTime): Boolean =
    dob.plusMonths(numberOfMonths).isAfter(DateTime.now())
}

trait Recently {
  def recently(numberOfMonths: Int, dose: DateTime): Boolean =
    DateTime.now().plusMonths(-numberOfMonths).isBefore(dose)
  def recently(numberOfMonths: Int, dose: Option[DateTime]): Boolean =
    dose match {
      case None => false
      case Some(dose) => DateTime.now().plusMonths(-numberOfMonths).isBefore(dose)
    }
}

trait NewBorn {
  // Determine if the child is new born.
  def isNewBorn (dob: DateTime): Boolean = dob.plusMonths(2).isAfter(DateTime.now())
}


// Common operations on the doses given.
class Doses(name: String, dob: DateTime, doses: Array[Option[DateTime]]) extends AgeRange {
  // Compute the number of doses of the vaccine given.
  val currentDate = new DateTime() // For some reason this give the current time.
  def numberOfDoses(doses: Array[Option[DateTime]]): Int = doses.count(d => !d.isEmpty)

 def doseNwithinAgePeriod(dose: Int, startMonth: Int, endMonth: Int): Boolean =
    withinRange(doses(dose), dob, startMonth, endMonth)
}

// Vaccinations
// Example names: DTAP, with maximum of 5 shots (vaccinations).
//                Polio, with maximum of 5 shots (vaccinations).
class Vaccine(name: String, max: Int) extends VaccineStatus {
  def fillDoses(doses: Array[DateTime], max: Int): Array[DateTime] = {
    for (j <- 0 to 10) { doses(j) = null }
    return null
  }
  val doses: Array[DateTime] = fillDoses(doses, max)
  println ("doses.length: ", doses.length)
}

class DTAP (dob: DateTime, doses: Array[Option[DateTime]]) extends Doses("DTAP", dob, doses) with NewBorn with Recently {
  numberOfDoses(doses) match {
    case 0 => isNewBorn(dob)
  }
}

// ----------------------------------------------------------------------------
object HelloWorld {
  def multiply(m: Int)(n: Int): Int = m * n
  def timesTwo(m: Int) = multiply(2)(_)
  def adder(m: Int, n: Int): Int = m + n
  val curriedAdd = (adder _).curried
  val addTwo = curriedAdd(2)
  val hp20b = Calculator("HP", "20b")
  val hp20B = Calculator("HP", "20b")
  val hostPort = ("localhost", 80)
  val hostPort2 = "localhost" -> 80 // Special sauce for tuples.
  val map1 = Map("timesTwo" -> { timesTwo(_) }) // Map with function as value.
  val numbers = Map("one" -> 1, "two" -> 2)
  val numberz = List(1, 2, 3, 4)
  val zipl = List(1, 2, 3).zip(List("a", "b", "c"))

  numberz.foreach((i: Int) => i * 2) // foreach as map, return value is Unit.

  def calcType(calc: Calculator) = calc match {
    case Calculator("HP", "20B") => "financial"
    case Calculator("HP", "48G") => "scientific"
    case Calculator("HP", "30B") => "business"
    case Calculator(ourBrand, ourModel) => "Calculator: %s %s is of unknown type".format(ourBrand, ourModel)
}

  // Pattern match treating different types differently.
  def bigger(o: Any): Any = {
    o match {
      case i: Int if i < 0 => i - 1
      case i: Int => i + 1
      case d: Double if d < 0.0 => d - 0.1
      case d: Double => d + 0.1
      case text: String => text + "s"
    }
  }

  // Functional Composition
  def f(s: String) = "f(" + s + ")"
  def g(s: String) = "g(" + s + ")"
  val fComposeG = f _ compose g _
  val fAndThenG = f _ andThen g _

  // So just what are case statements?
  // It’s a subclass of function called a PartialFunction.
  // What is a collection of multiple case statements?
  // They are multiple PartialFunctions composed together.
  // isDefinedAt is a method on PartialFunction that can be used to determine
  // if the PartialFunction will accept a given argument.
  val one: PartialFunction[Int, String] = { case 1 => "one" }
  // PartialFunctions can be composed with something new, called orElse,
  // that reflects whether the PartialFunction is defined over the supplied argument.
  val two: PartialFunction[Int, String] = { case 2 => "two" }
  val three: PartialFunction[Int, String] = { case 3 => "three" }
  val wildcard: PartialFunction[Int, String] = { case _ => "something else" }
  val partial = one orElse two orElse three orElse wildcard

  // Types
  // List with different types.
  val gronkList = 2 :: 1 :: "bar" :: "foo" :: Nil
  def drop1[A](l: List[A]) = l.tail // A polymorphic function.
  // Bounds on class.
  class Animal { val sound = "rustle" }
  class Bird extends Animal { override val sound = "call" }
  class Chicken extends Bird { override val sound = "cluck" }
  // Function parameters are contravariant.
  val getTweet: (Bird => String) = ((a: Animal) => a.sound )
  // Functions return type is covariant.
  def cacophony[T <: Animal](things: Seq[T]) = things map (_.sound) // Cacaphony of sounds.
  def biophony[T <: Animal](things: Seq[T]) = things map (_.sound)
  val bioSounds = biophony(Seq(new Chicken, new Bird))
  val flock = List(new Bird, new Bird)
  val hatch = new Chicken :: flock
  // Advanced Types
  implicit def strToInt(x: String) = x.toInt
  val y1 : Int = "123" // Now we can get away with this.
  val y2 : Int = math.max("123", 111)
  class Container[A <% Int] { def addIt(x: A) = 123 + x }
  val y3 : Int = (new Container[String]).addIt("123")
  val y4 : Int = (new Container[Int]).addIt(123)

  def main(args: Array[String]): Unit = {
    println("fComposeG(\"yay\") = " + fComposeG("yay"))
    println("fAndThenG(\"yay\") = " + fAndThenG("yay"))
    println("Difference of the value is: " + funSub());
    println("Difference of the value is: " + funSub(y = 6, x = 8));
    println("zipl = " + zipl)
    println( "Testing hp20b == hp20B: " + (hp20b == hp20B) )
    println( "Bigger 7.0 ", bigger(7.0))
    println( "hostPort = (" + hostPort._1 + ", " + hostPort._2 + ")")
    println( "hostPort2 = (" + hostPort2._1 + ", " + hostPort2._2 + ")")
    println( "numbers.get(\"two\")", numbers.get("two"), "numbers.get(\"three\")", numbers.get("three"))
    println( "numbers.get(\"two\").getOrElse(0) * 2 = " + (numbers.get("two").getOrElse(0) * 2))
    val ss1 = variableArgument.printAll ("Scala", "is", "great")
    val ss2 = variableArgument.capitalizeAll("Scala", "is", "great")
    println(ss2)
    println("2 times 2 is ", timesTwo(2))
    println("2 plus 2 is ", addTwo(2))
    println("one.isDefinedAt(1) = " + one.isDefinedAt(1))
    println("one.isDefinedAt(2) = " + one.isDefinedAt(2))
    println("partial(2) = " + partial(2))
    println("partial(8) = " + partial(8))
    println("gronkList = " + gronkList)
    println("bioSounds = " + bioSounds)
    println("hatch = " + hatch)
    println("y1 = " + y1)
    println("y2 = " + y2)
    println("y3 = " + y3)
    println("y4 = " + y4)
    // Only works for scalac
    // println("the color is: " + com.twitter.example.colorHolder.BLUE)
    // val creds = Credentials(accessToken, refreshToken, clientId, clientSecret)
    // val io = for {
    //   service <- GSheetsService[IO](creds)
    //   res <- service.updateAndGet
    // } yield res
    // io.unsafeRunSync()

    // JSON
    val source = """{ "some": "JSON source", "other": "XML" }"""
    val jsonAst: spray.json.JsValue = source.parseJson // or JsonParser(source)
    val json = jsonAst.prettyPrint // or .compactPrint
    val jsonAst2 = List(1, 2, 3).toJson
    val myObject = jsonAst.convertTo[Map[String,String]]
    println("\nsource = " + source)
    println("jsonAst = " + jsonAst)
    println("json = " + json)
    println("jsonAst2 = " + jsonAst2)
    println("myObject = " + myObject)
    println("myObject(\"some\") = " + myObject("some"))

    val filename = "fileopen.scala"
    println("------ > inputs/ImmunizationData.json\n")
    var j = 0
    for (line <- fromFile("inputs/ImmunizationData.json").getLines) {
      if (j < 12) {
        println(line)
        j = j + 1
      }
    }
  }
}

// List<String> SCOPES_ARRAY = Arrays.asList(
//         "https://www.googleapis.com/auth/drive.file",
//         "https://www.googleapis.com/auth/userinfo.email",
//         "https://www.googleapis.com/auth/userinfo.profile",
//         "https://docs.google.com/feeds",
//         "https://spreadsheets.google.com/feeds");
// http://blog.pamelafox.org/2013/06/exporting-google-spreadsheet-as-json.html
