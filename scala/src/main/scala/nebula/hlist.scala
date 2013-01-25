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

object HListUtils {
  def liftA2[HF, X <: HList, Y <: HList, Out <: HList](hf: HF)(x: X, y: Y)(implicit lift: LiftA2[HF, X, Y, Out]) = lift(x, y)
  
  def mkTuple2[A <: HList, B <: HList, Out <: HList](a: A, b: B)(implicit lift: LiftA2[tuple2.type,A,B,Out]) = liftA2(tuple2)(a, b)
}
