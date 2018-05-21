package fpinscala.stream
import Stream._

sealed trait Stream[+A]{
	def toList: List[A] = this match {
		case Empty => List()
		case Cons(h, t) => h() :: t().toList
	}

	def toListStackSafe: List[A] = {
		@annotation.tailrec
		def go(s: Stream[A], acc: List[A]): List[A] = s match {
			case Empty => acc
			case Cons(h, t) => go(t(), h() :: acc)
		}
		go(this, List()).reverse
	}

    // this is wrong.. the order is revesed... but stacksafe XD
	def takeWrong(n: Int): Stream[A] = {
		@annotation.tailrec
		def go(s: Stream[A], acc: Stream[A], n: Int): Stream[A] = {
			if(n == 0) acc
			else s match {
				case Empty => acc
				case Cons(h, t) => go(t(), cons(h(), acc), n-1)
			}
		}
		go(this, empty, n)
	} 

    // this is wrong.. the order is revesed... but stacksafe XD
	def takeStillWrong(n: Int): Stream[A] = {
		@annotation.tailrec
		def go(s: Stream[A], acc: Stream[A], n: Int): Stream[A] =  s match {
			case Cons(h, t) if n > 0 => go(t(), cons(h(), acc), n-1)
		    case _ => acc
		}
		go(this, empty, n)
	}

    // not stacksafe through...
	def take(n: Int): Stream[A] = this match {
		case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
		case Cons(h, _) if n == 1 => cons(h(), empty)
		case Empty => Empty
	}

	def drop(n: Int): Stream[A] = this match {
		case Cons(_, t) if n > 0 => t().drop(n-1)
		case Cons(_, t) if n == 0 => this
	}

	def takeWhile(p: A => Boolean): Stream[A] = this match {
		case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
		case _ => Empty
	}

	def exists(p: A => Boolean): Boolean =
        foldRight(false)((a, b) => p(a) || b)

	def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
		case Cons(h, t) => f(h(), t().foldRight(z)(f))
		case _ => z
	}

	def forAll(p: A => Boolean): Boolean =
		foldRight(true)((a, b) => p(a) && b)

	def takeWhileWithFoldR(p: A => Boolean): Stream[A] =
		foldRight(empty[A])((h, t) => if(p(h)) cons(h, t) else Empty)

	def map[B](f: A => B): Stream[B] = 
		foldRight(empty[B])((h, t) => cons(f(h), t))

	def mapUnfold[B](f: A => B): Stream[B] = unfold(this){ 
		case Empty => None
		case Cons(h,t) => Some((f(h()), t()))
	}

	def takeUnford(n: Int): Stream[A] = unfold((this,n)){
		case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n-1))
		case _ => None
	}

	def takeWhileUnford(p: A => Boolean): Stream[A] = unfold(this) {
		case Cons(h, t) if p(h()) => Some(h(), t())
		case _ => None
	}

	def zipWith[B,C](b: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, b)) {
		case (Cons(ah, at), Cons(bh, bt)) => Some((f(ah(), bh()), (at(), bt())))
		case _ => None
	}

	def zip[B](s2: Stream[B]): Stream[(A,B)] =
    	zipWith(s2)((_,_))
 

	def zipAll[B](b: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, b)) {
		case (Cons(ah, at), Cons(bh, bt)) => Some((Some(ah()),  Some(bh())), (at(), bt()))
		case (Empty, Cons(bh, bt)) =>        Some((None,        Some(bh())), (empty[A], bt()))
		case (Cons(ah, at), Empty) =>        Some((Some(ah()),  None),       (at(), empty[B]))
		case (_, _) => None
	}

	def tails: Stream[Stream[A]] =
		unfold(this) {
			case Cons(h,t) => Some((this, t().drop(1)))
			case _ => None
		}

	@annotation.tailrec
    final def find(f: A => Boolean): Option[A] = this match {
    	case Empty => None
    	case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
 	}


	def filter(f: A => Boolean): Stream[A] = 
		foldRight(empty[A])((h, t) => if(f(h)) cons(h,t) else t)
    
    // don't understand why I need type B as a super type of A?
	def append[B>:A](s2: Stream[B]): Stream[B] =
		foldRight(s2)((h, t) => cons(h, t))

	def flatMap[B](f: A => Stream[B]) : Stream[B] = 
		foldRight(empty[B])((h, t) => f(h).append(t))

	def headOption: Option[A] =
        foldRight(None: Option[A])((h,_) => Some(h))
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
	def constant[A](a: A): Stream[A] = cons(a, constant(a))

	def from(n: Int): Stream[Int] = cons(n, from(n+1))

	def fibs: Stream[Int] = {
		def helper(a: Int, b: Int): Stream[Int] = {
			cons(a, helper(b, a+b))
		}
		helper(0,1)
	}

	def unfold[A, S](z: S)(f: S => Option[(A,S)]): Stream[A] = {
    	f(z) match {
			case None => empty
			case Some((a, s)) => cons(a, unfold(s)(f))
		}
	}

	def constantUnfold[A](a: A): Stream[A] = unfold(a)(z => Some((z, z))) 

	def fromUnfold(n: Int): Stream[Int] = unfold(n)(z => Some((z, z+1)))

	def fibsUnfold: Stream[Int] = unfold((0,1)) { case (a, b) => Some(a, (b, a+b)) }


	def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
		lazy val head = hd
		lazy val tail = tl
		Cons(() => head, () => tail)
	}

	def empty[A]: Stream[A] = Empty

	def apply[A](as: A*): Stream[A] =
		if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}