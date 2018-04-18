package fpinscala.datasctructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
  	case Leaf(_) => 1
  	case Branch(left, right) => size(left) + size(right)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
  	case Leaf(v) => v
  	case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
  	case Leaf(_) => 0
  	case Branch(left, right) => depth(left) + 1 max depth(right) + 1
  }

  def map[A, B](tree: Tree[A])(f: A => B ): Tree[B] = tree match {
  	case Leaf(x) => Leaf(f(x))
  	case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
  	case Leaf(x) => f(x)
  	case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def size2[A](tree: Tree[A]) =
  	fold(tree)((_) => 1)(_ + _)

  def maximum2(tree: Tree[Int]) =
  	fold(tree)((x) => x)(_ max _)

  def depth2[A](tree: Tree[A]) =
  	fold(tree)((_) => 0)(_ max _)

  def map2[A, B](tree: Tree[A])(f: A => B ) = 
  	fold(tree)((x) => Leaf(f(x)): Tree[B])((x,y) => Branch(x,y))
}