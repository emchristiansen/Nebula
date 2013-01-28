package nebula

import shapeless._

/**
 * From http://stackoverflow.com/questions/14447487/creating-an-hlist-of-all-pairs-from-two-hlists
 */
object tuple2 extends Poly2 {
  implicit def whatever[A, B] = at[A, B] { case (a, b) => (a, b) }
}

trait ApplyMapper[HF, A, X <: HList, Out <: HList] {
  def apply(a: A, x: X): Out
}

object ApplyMapper {
  implicit def hnil[HF, A] = new ApplyMapper[HF, A, HNil, HNil] {
    def apply(a: A, x: HNil) = HNil
  }
  implicit def hlist[HF, A, XH, XT <: HList, OutH, OutT <: HList](implicit pb: Poly.Pullback2Aux[HF, A, XH, OutH],
    am: ApplyMapper[HF, A, XT, OutT]) = new ApplyMapper[HF, A, XH :: XT, OutH :: OutT] {
    def apply(a: A, x: XH :: XT) = pb(a, x.head) :: am(a, x.tail)
  }
}

trait LiftA2[HF, X <: HList, Y <: HList, Out <: HList] {
  def apply(x: X, y: Y): Out
}

object LiftA2 {
  implicit def hnil[HF, Y <: HList] = new LiftA2[HF, HNil, Y, HNil] {
    def apply(x: HNil, y: Y) = HNil
  }

  implicit def hlist[HF, XH, XT <: HList, Y <: HList, Out1 <: HList, Out2 <: HList, Out <: HList](implicit am: ApplyMapper[HF, XH, Y, Out1],
    lift: LiftA2[HF, XT, Y, Out2],
    prepend: PrependAux[Out1, Out2, Out]) = new LiftA2[HF, XH :: XT, Y, Out] {
    def apply(x: XH :: XT, y: Y) = prepend(am(x.head, y), lift(x.tail, y))
  }
}

object HListUtil {
  def liftA2[HF, X <: HList, Y <: HList, Out <: HList](hf: HF)(x: X, y: Y)(implicit lift: LiftA2[HF, X, Y, Out]) = lift(x, y)

  ///////////////////////////////////////////////////////////

  def mkTuple2[A <: HList, B <: HList, Out <: HList](a: A, b: B)(implicit lift: LiftA2[tuple2.type, A, B, Out]) = liftA2(tuple2)(a, b)

  ///////////////////////////////////////////////////////////

  object Flatten3 extends Poly1 {
    implicit def default[A, B, C] = at[((A, B), C)] { case ((a, b), c) => (a, b, c) }
  }

  def mkTuple3[A <: HList, B <: HList, C <: HList, Out1 <: HList, Out2 <: HList](
    a: A,
    b: B,
    c: C)(
      implicit lift1: LiftA2[tuple2.type, A, B, Out1],
      lift2: LiftA2[tuple2.type, Out1, C, Out2],
      mapper: Mapper[Flatten3.type, Out2]) = {
    val ab = mkTuple2(a, b)
    val tuples = mkTuple2(ab, c)

    tuples map Flatten3
  }

  ///////////////////////////////////////////////////////////

  object Flatten4 extends Poly1 {
    implicit def default[A, B, C, D] = at[(((A, B), C), D)] { case (((a, b), c), d) => (a, b, c, d) }
  }

  def mkTuple4[A <: HList, B <: HList, C <: HList, D <: HList, Out1 <: HList, Out2 <: HList, Out3 <: HList](
    a: A,
    b: B,
    c: C,
    d: D)(
      implicit lift1: LiftA2[tuple2.type, A, B, Out1],
      lift2: LiftA2[tuple2.type, Out1, C, Out2],
      lift3: LiftA2[tuple2.type, Out2, D, Out3],
      mapper: Mapper[Flatten4.type, Out3]) = {
    val ab = mkTuple2(a, b)
    val abc = mkTuple2(ab, c)
    val abcd = mkTuple2(abc, d)

    abcd map Flatten4
  }
}
