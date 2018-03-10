package laziness

import Stream._

import scala.annotation.tailrec

trait Stream[+A] {
  def toList: List[A] = {
    @tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty      => acc
      case Cons(h, t) => go(t(), h() :: acc)
    }
    go(this, List.empty).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Empty                => empty
  }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1)          => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _                        => None
    }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _                    => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) =>
        f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhile_1(f: A => Boolean): Stream[A] =
    foldRight(empty[A])(
      (h, t) =>
        if (f(h)) cons(h, t)
        else empty)

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some(f(h()), t())
    case Empty      => None
  }

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])(
      (h, t) =>
        if (f(h)) cons(h, t)
        else t)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  val fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None         => empty
      case Some((h, s)) => cons(h, unfold(s)(f))
    }
  }

  val fibsViaUnfold: Stream[Int] = unfold((0, 1)) {
    case (f0, f1) => Some((f0, (f1, f0 + f1)))
  }

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(n => Some(n, n + 1))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  val onesViaUnfold: Stream[Int] = unfold(1)(_ => Some((1, 1)))
}
