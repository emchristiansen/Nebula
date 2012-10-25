package nebula.util

///////////////////////////////////////////////////////////

// A function memoizer, adapted from
// http://stackoverflow.com/questions/5875767/scala-memoize-a-function-no-matter-how-many-arguments-the-function-takes

/**
 * A type class that can tuple and untuple function types.
 * @param [U] an untupled function type
 * @param [T] a tupled function type
 */
sealed class Tupler[U, T](val tupled: U => T,
                          val untupled: T => U)

object Tupler {
  implicit def function0[R]: Tupler[() => R, Unit => R] =
    new Tupler((f: () => R) => (_: Unit) => f(),
      (f: Unit => R) => () => f(()))
  implicit def function1[T, R]: Tupler[T => R, T => R] =
    new Tupler(identity, identity)
  implicit def function2[T1, T2, R]: Tupler[(T1, T2) => R, ((T1, T2)) => R] =
    new Tupler(_.tupled, Function.untupled[T1, T2, R])
  // ... more tuplers
}

trait FunctionDecorator {
  final def apply[T, R, F](f: F)(implicit e: Tupler[F, T => R]): F =
    e.untupled(decorate(e.tupled(f)))

  protected def decorate[T, R](f: T => R): T => R
}

/**
 * A memoized unary function.
 *
 * @param f A unary function to memoize
 * @param [T] the argument type
 *  * @param [R] the return type
 */
// TODO: Ideally, the signature is Memoize1[-T, +R] ...
class Memoize1[T, R](f: T => R) extends (T => R) {
  val cache = collection.mutable.HashMap[T, R]()

  // memoization implementation
  override def apply(t: T): R = {
    if (cache.contains(t)) {
      cache(t)
    } else {
      val r = f(t)
      cache += t -> r
      r
    }
  }
}

object Memoize extends FunctionDecorator {
  /**
   * Memoize a function.
   *
   * @param f the function to memoize
   */
  protected def decorate[T, R](f: T => R) = new Memoize1(f)
}
