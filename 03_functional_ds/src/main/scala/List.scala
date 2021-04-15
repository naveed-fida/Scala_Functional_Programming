package fpinscala.datastructures

sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int =
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

  def sumTailRec(ints: List[Int]): Int = {
    @annotation.tailrec
    def go(sum: Int, l: List[Int]): Int = l match {
      case Nil => sum
      case Cons(h, t) => go(sum + h, t);
    }

    go(0, ints);
  }

  def product(ds: List[Double]): Double =
    ds match {
      case Nil => 1
      case Cons(x, xs) => x * product(xs)
    }

  def foldRight[A, B](as: List[A], b: B) (f: (A, B) => B): B =
    as match {
      case Nil => b
      case Cons(x, xs) => f(x, foldRight(xs, b)(f))
    }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  def product2(ds: List[Double]): Double =
    foldRight(ds, 1.0)((x, y) => x * y)

  def sum2(is: List[Int]): Int =
    foldRight(is, 0)((x, y) => x + y)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, acc) => 1 + acc)

  def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)
  def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc,_) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,h) => Cons(h,acc))

  def tail[A](xs: List[A]): List[A] =
    xs match {
      case Nil => sys.error("tail on empty list")
      case Cons(_, t) => t
    }

  def setHead[A](xs: List[A], h: A): List[A] =
    xs match{
      case Nil => sys.error("setHead on empty list")
      case Cons(_, t) => Cons(h, t)
    }

  def drop[A](xs: List[A], n: Int): List[A] = {
    if (n <= 0) xs
    else xs match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  // using a feature called pattern guard
  def dropWhile[A](xs: List[A], f: A => Boolean): List[A] =
    xs match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => xs
    }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def initial[A](xs: List[A]): List[A] =
    xs match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, initial(t))
    }

  def initial2[A](xs: List[A]): List[A] = {
    import collection.immutable.Queue

    @annotation.tailrec
    def go(curr: List[A], q: Queue[A]): List[A] = curr match {
      case Nil => Nil
      case Cons(_, Nil) => List(q.toList: _*)
      case Cons(h, t) => go(t, q.enqueue(h))
    }

    go(xs, Queue[A]());
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def toString[A](xs: List[A]): String = {
    @annotation.tailrec
    def loop(str: String, ys: List[A]): String =
      ys match {
        case Nil => str
        case Cons(h, t) => loop(str + (if (str.isEmpty()) "" else ", ") + h.toString, t)
      }

    loop("", xs)
  }

}
