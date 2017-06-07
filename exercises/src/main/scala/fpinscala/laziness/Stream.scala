package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h())  => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)
  /*this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }*/

  def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatMap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](p: A => B): Stream[B] = foldRight(Empty: Stream[B])((a, b) => Cons(() => p(a), () => b))
  /*this match {
    case Cons(h, t) => Cons(() => p(h()), () => t().map(p))
    case _ => Empty
  }*/

  def filter(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((a, b) => if (p(a)) Cons(() => a, () => b.filter(p)) else b.filter(p))
  /*this match {
    case Cons(h, t) => if (p(h())) Cons(h, () => t().filter(p)) else t().filter(p)
    case _ => Empty
  }*/

  def append[B>:A](a: Stream[B]): Stream[B] = foldRight(a)((h, t) => Cons(() => h, () => t))
  /*this match {
    case Empty => a
    case Cons(h, t) => Cons(h, () => t().append(a))
  }*/

  def flatMap[B](p: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((a, b) => p(a).append(b))
  /*this match {
    case Cons(h, t) => p(h()).append(t())
    case Empty => Empty
  }*/

  // exercise 5.13
  def map1[B](p: A => B): Stream[B] = unfold(this){
    case Cons(h, t) => Some(p(h()), t())
    case _ => None
  }

  def take1(n: Int): Stream[A] = unfold((this, n)) {
    case (_, 0) => None
    case (Cons(h, t), n) => Some((h(), (t(), n - 1)))
  }

  def takeWhile1(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if (p(h())) => Some((h(), t()))
    case _ => None
  }

  def zipWith[B, C](a: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, a)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zipAll[B](a: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, a)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
    case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
    case (Empty, Empty) => None
  }

  def startsWith[B](s: Stream[B]): Boolean = zipAll(s).takeWhile(!_._2.isEmpty).forAll{case (a, b) => a == b}

  // exercise 5.15
  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case s => Some(s, s.drop(1))
  } append Empty

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    foldRight((z, Stream(z))) {
      case (a, (b, c)) =>
        val acc = f(a, b)
        (acc, Cons(() => acc, () => c))
    }._2
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def main(args: Array[String]): Unit = {
    val s = Cons[Int](() => 1, () => Cons[Int](() => 2, () => Empty))

    assert(s.toList == List(1, 2))
    assert(s.take(1).toList == List(1))
    assert(s.drop(1).toList == List(2))
    assert(s.takeWhile(x => x == 1).toList == List(1))
    assert(s.forAll(x => x < 3))

    assert(s.map(_ + 1).toList == List(2, 3))
    assert(s.map(_.toString).toList == List("1", "2"))
    assert(s.filter(x => x == 1).toList == List(1))

    val s1 = Stream(1, 2, 3, 4)

    println(s1.map(_ + 10).filter(_ % 2 == 0).toList)
    println(fibs().take(10).toList)

    println(s1.take1(2).toList)

    s1.tails.toList.foreach(s => print(s.toList + "\t"))
    println()

    println(s1.scanRight(0)(_ + _).toList)
  }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def next(a: Int, b: Int): Stream[Int] = {
      cons(a, next(b, a + b))
    }

    next(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  // exercise 5.12
  def fibs1(): Stream[Int] = unfold((0, 1)){ case (a, b) => Some((a, (b, a + b))) }

  def from1(n: Int): Stream[Int] = unfold(n)(m => Some(m, m + 1))

  def constant1[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  val ones1: Stream[Int] = unfold(1)(_ => Some(1, 1))

}
