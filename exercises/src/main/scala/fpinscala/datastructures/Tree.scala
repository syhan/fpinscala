package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): (Int, Int) = {
    def go(t: Tree[A], acc: (Int, Int)): (Int, Int) = {
      t match {
        case Leaf(_) => (acc._1 + 1, acc._2)
        case Branch(l, r) => {
          val (x1, y1) = go(l, acc)
          val (x2, y2) = go(r, acc)

          (x1 + x2, y1 + y2 + 1)
        }
      }
    }

    go(tree, (0, 0))
  }

  def maximum(tree: Tree[Int]): Int = {
    def go(t: Tree[Int], m: Int): Int = {
      t match {
        case Leaf(v) => m max v
        case Branch(l, r) => go(l, m) max go(r, m)
      }
    }

    go(tree, Int.MinValue)
  }

  def depth[A](tree: Tree[A]): Int = {
    def go(t: Tree[A], d: Int): Int = {
      t match {
        case Leaf(_) => d + 1
        case Branch(l, r) => go(l, d) max go(r, d)
      }

    }

    go(tree, 0)
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }


}