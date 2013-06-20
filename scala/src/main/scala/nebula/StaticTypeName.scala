package nebula

///////////////////////////////////////////////

import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.collection.mutable.{ ListBuffer, Stack }

object StaticTypeName {
  //  def typeNameFromInstance[A](instance: A): String = macro typeNameFromInstanceImplementation[A]
  //
  //  def typeNameFromInstanceImplementation[A](
  //    c: Context)(
  //      instance: c.Expr[A]): c.Expr[String] = {
  //    import c.universe._
  //
  //    val name = instance.actualType.toString
  //    c.Expr[String](Literal(Constant(name)))
  //  }

  //  def typeNameNoInstance[A]: String = macro typeNameNoInstanceImplementation[A]
  //
  //  def typeNameNoInstanceImplementation[A](c: Context): c.Expr[String] = {
  //    import c.universe._
  //
  //    val TypeApply(_, List(typeTree)) = c.macroApplication
  //    c.literal(typeTree.toString())
  //  }

  //  def typeNameNoInstance[A]: nebula.TypeName[A] = macro typeNameNoInstanceImplementation[A]
  //
  //  def typeNameNoInstanceImplementation[A](c: Context): c.Expr[nebula.TypeName[A]] = {
  //    import c.universe._
  //
  //    val TypeApply(_, List(typeTree)) = c.macroApplication
  //    val typeString = typeTree.toString
  //    //    val expr = c.Expr(
  //    //        Apply(
  //    //          TypeApply(
  //    //            Select(
  //    //              Ident("nebula.TypeName"),
  //    //              newTermName("apply")),
  //    //            List(Ident("A"))),
  //    //          List(Literal(Constant(typeString)))))
  //    c.Expr[nebula.TypeName[A]](
  //      Apply(
  //        TypeApply(
  //          Select(
  //            Ident("nebula.TypeName"),
  //            newTermName("apply")),
  //          List(Ident("A"))),
  //        List(Literal(Constant(typeString)))))
  //
  //    //    (nebula.TypeName.apply[A](typeTree.toString))
  //    //    val typeName = TypeName[A](typeTree.toString)
  //    //        reify(TypeName[A](typeString))
  //    //    c.literal(typeTree.toString())
  //  }

  //  def printType[A] = macro printType_impl[A]
  //
  //  def printType_impl[A: c.WeakTypeTag](c: Context): c.Expr[Unit] = {
  //    c.universe.reify(println(typeStringNoInstanceImplementation[A](c).splice))
  //  }

  /////////////////////

  def typeStringFromConcrete[A]: String = macro typeStringFromConcrete_impl[A]

  def typeStringFromConcrete_impl[A: c.WeakTypeTag](
    c: Context): c.Expr[String] = {
    import c.universe._

    c.literal(weakTypeOf[A].toString)
  }
  
  ////////////////////////////
  
//  implicit def typeStringFromConcreteInstanceTODO[A](a: A): TypeString = 
//    macro typeStringFromConcreteInstanceTODO_impl[A]
//  
//  def typeStringFromConcreteInstanceTODO_impl[A: c.WeakTypeTag](
//      c: Context)(a: A): c.Expr[TypeString] = {
//    val typeString = typeStringFromConcrete_impl[A](c)
//    c.universe.reify(nebula.TypeString(typeString.splice))
//  }

  /////////////////////////////////

  def typeNameFromConcrete[A]: TypeName[A] = 
    macro typeNameFromConcrete_impl[A]

  def typeNameFromConcrete_impl[A: c.WeakTypeTag](
      c: Context): c.Expr[TypeName[A]] = {
    val typeString = typeStringFromConcrete_impl[A](c)
    c.universe.reify(nebula.TypeName[A](typeString.splice))
  }  
  
  /////////////////////////////////

  def typeNameFromConcreteInstance[A](a: A): TypeName[A] = 
    macro typeNameFromConcreteInstance_impl[A]
  
  def typeNameFromConcreteInstance_impl[A: c.WeakTypeTag](
      c: Context)(a: A): c.Expr[TypeName[A]] = 
    typeNameFromConcrete_impl[A](c)
  
  /////////////////////////

  
  
//  implicit def implicitFoo[A]: Foo[A] = macro implicitFoo_impl[A]
//  
//  def implicitFoo_impl[A](c: Context): c.Expr[Foo[A]] =
//    c.universe.reify(Foo[A])
    
//  implicit def implicitFoo[A]: Foo[A] = ???
   
  
//  implicit def implicitInt: Int = macro implicitInt_impl
//  
//  def implicitInt_impl(c: Context): c.Expr[Int] = c.universe.reify(42)
}