package fpinscala.datasctructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
	def sum(ints: List[Int]): Int = ints match {
		case Nil => 0
		case Cons(x,xs) => x + sum(xs)
	}

	def product(ds: List[Double]): Double = ds match {
		case Nil => 1.0
		case Cons(0.0, _) => 0.0
		case Cons(x,xs) => x * product(xs)
	}

	def apply[A](as: A*): List[A] =
		if (as.isEmpty) Nil
	  else Cons(as.head, apply(as.tail:_*))

	/*e3_2*/
  def tail[A](ls: List[A]): List[A] = ls match {
  	case Nil => Nil
  	case Cons(_,xs) => xs
  }

  /*e3_3*/
  def setHead[A](ls: List[A], head: A): List[A] = ls match {
  	case Nil => List(head)
  	case Cons(_,xs) => Cons(head, xs)
  }

  /*e3_4*/
  def drop[A](ls: List[A], n: Int): List[A] = {
  	if (n <= 0) ls
  	else ls match {
      case Nil => Nil
      case Cons(_,xs) => drop(xs, n-1)
    }	
  }

  /*e3_5*/
  def dropWhile[A](ls: List[A], f: A => Boolean): List[A] = ls match {
  	case Cons(x,xs) if (f(x)) => dropWhile(xs,f)
  	case _ => ls
  }

  /*e3_6*/
  def init[A](ls: List[A]): List[A] = ls match {
  	case Cons(x,xs) if (xs != Nil) => Cons(x, init(xs))
  	case _ => Nil
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B) : B =
  	as match {
  		case Nil => z
  		case Cons(x, xs) => f(x, foldRight(xs, z)(f)) 
  	}

  def sum2(ns: List[Int]) = 
  	foldRight(ns, 0)((x,y)=>(x+y))

  def product2(ns: List[Double]) = 
  	foldRight(ns, 1.0)((x,y)=>(x*y))

  def e3_8 = foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

  /* e3_9 */
  def length[A](as: List[A]): Int = as match {
  	case Nil => 0
  	case Cons(_, xs) => 1 + length(xs)
  }

  /* e3_10 */
  def foldLeft[A, B](as: List[A], z: B)(f: (B,A)=> B): B = as match {
  	case Nil => z
  	case Cons(x,xs) => foldLeft(xs, f(z, x))(f)
  }

  def length2[A](ns: List[A]) =
  	foldLeft(ns, 0)((z,x)=>(z+1))

  /* e3-12 */
  def reverse[A](ns: List[A]) =
  	foldLeft(ns, Nil:List[A])((x,y)=>Cons(y,x))

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), z)((b,a) => f(a,b))

  /* e3-14 */
  def append1[A](s1: List[A], s2: List[A]) : List[A] = s1 match {
    case Nil => s2
    case Cons(x, xs) => Cons(x, append1(xs, s2))
  }

  def append2[A](s1: List[A], s2: List[A]) = 
    foldRight(s1, s2)(Cons(_, _))

  def append3[A](s1: List[A], s2: List[A]): List[A] = 
    foldRightViaFoldLeft(s1, s2)(Cons(_, _))

  /* e3-15 */
  def concat[A](ll: List[List[A]]) : List[A] = {
    def helper[A](ll: List[List[A]], result: List[A]) : List[A] = ll match {
      case Nil => result
      case Cons(l, ls) => l match {
        case Nil => helper(ls, result)
        case Cons(x, xs) => helper(Cons(xs, ls), Cons(x, result))
      }
    }
    reverse(helper(ll,Nil))
  }

  def concat2[A](ll: List[List[A]]) =
    foldLeft(ll, Nil:List[A])((x,y)=> append1(x,y))

  /* e3_16 */
  def add1(l: List[Int]): List[Int] = {
    def helper(l: List[Int], result: List[Int]): List[Int] = l match {
      case Nil => result
      case Cons(i, is) => helper(is, Cons(i+1, result))
    }
    helper(reverse(l), Nil)
  }

  /* e3_17 */
  def toString(ld: List[Double]): List[String] = {
    def helper(ld: List[Double], result: List[String]): List[String] = ld match {
      case Nil => result
      case Cons(d, ds) => helper(ds, Cons(d.toString, result))
    }
    helper(reverse(ld), Nil)
  }

  /* e3_18 */
  def map[A, B](as: List[A])(f: A=>B): List[B] = {
    def helper(as: List[A], result: List[B]): List[B] = as match {
      case Nil => result
      case Cons(x, xs) => helper(xs, Cons(f(x), result))
    }
    helper(reverse(as), Nil)
  }

  def add12(l: List[Int]) =
    map(l)((x)=>x+1)

  /* e3_19 */
  def filter[A](l: List[A])(f: A=>Boolean): List[A] = {
    def helper(as: List[A], result: List[A]): List[A] = as match {
      case Nil => result
      case Cons(x, xs) if f(x) => helper(xs, Cons(x, result))
      case Cons(_, xs) => helper(xs, result)
    }
    helper(reverse(l), Nil)
  }
  
  /* e3_20 */
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    def helper(as: List[A], result: List[List[B]]): List[List[B]] = as match {
      case Nil => result
      case Cons(x, xs) => helper(xs, Cons(f(x), result))
    }
    concat2(helper(reverse(as), Nil))
  }

  /* e3_21 */
  def filter2[A](l: List[A])(f: A=>Boolean) =
    flatMap(l)(x => if (f(x))  List(x) else Nil)

  /* tbc */

  /* e3_22 */
  def addList(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(l1a, l1s), Cons(l2a, l2s)) => Cons(l1a + l2a, addList(l1s, l2s))
  }

  


}

/*e3_1*/
val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(2, _))) => x /* no hit */
	  case Nil => 42 /* no hit */
	  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y /*hit, return 3*/
	  case _ => 101
}

