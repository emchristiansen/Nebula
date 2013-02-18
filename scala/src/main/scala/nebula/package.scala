import scala.reflect.runtime._
import scala.reflect.runtime
import scala.reflect._
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.runtime.universe.typeTag
import scala.util._
import spray.json._
import shapeless._
import java.io.File
import nebula.util._
import breeze.math._
import breeze.linalg._
import org.expecty.Expecty

///////////////////////////////////////////////////////////

package object nebula extends IO with Near with Eval {
  /**
   * Prints a string in a clearly delimited block.
   */
  def printBlock[S](string: S) = ???
  
//  def expecty = new Expecty()
//  def asserty = new Expecty()
//  def requirey = new Expecty()
  def expecty(x: => Boolean) = scala.Predef.assert(x)
  def asserty(x: => Boolean) = scala.Predef.assert(x)
  def requirey(x: => Boolean) = scala.Predef.require(x)
  
  /**
   * Convenience function for asserting something about an expression
   * and then returning the expression.
   */
  implicit class AddAssert[A](value: A) {
    def assert(assertions: (A => Boolean)*) = {
      assertions.map(assertion => Predef.assert(assertion(value)))
      value
    }
  }  
  
  /**
   * "5 times println("hello")" prints "hello" 5 times.
   */
  implicit class IntTimes(int: Int) {
    def times[A](function: => A): IndexedSeq[A] =
      (0 until int).map(_ => function)
  }  
  
  lazy val loadOpenCV = System.loadLibrary("opencv_java")

  /**
   * Quick syntax for implicit conversions.
   */
  implicit class AddTo[A](obj: A) {
    def to[B](implicit conversion: A => B): B = conversion(obj)
  }

    /**
   * Adds |compose| for functions of arity 2.
   */
  implicit class AddCompose[-T1, +R](function1: (T1) => R) {
    def compose[A, B](function2: (A, B) => T1): (A, B) => R = 
      (a, b) => function1(function2(a, b))
  }

}