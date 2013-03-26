package nebula

///////////////////////////////////////////////

import scala.reflect.macros.Context
import scala.collection.mutable.{ ListBuffer, Stack }

object StaticTypeName {
  def typeName[A](instance: A): String = macro typeNameImplementation[A]

  def typeNameImplementation[A](
    c: Context)(
      instance: c.Expr[A]): c.Expr[String] = {
    import c.universe._
    
    val name = instance.actualType.toString

    c.Expr[String](Literal(Constant(name)))
  }
}