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
	def take(n: Int): Stream[A] = {
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
	def takeBetter(n: Int): Stream[A] = {
		@annotation.tailrec
		def go(s: Stream[A], acc: Stream[A], n: Int): Stream[A] =  s match {
			case Cons(h, t) if n > 0 => go(t(), cons(h(), acc), n-1)
		    case _ => acc
		}
		go(this, empty, n)
	}

    // not stacksafe through...
	def takeRight(n: Int): Stream[A] = this match {
		case Cons(h, t) if n > 1 => cons(h(), t().takeRight(n-1))
		case Cons(h, _) if n == 1 => cons(h(), empty)
		case Empty => Empty
	}

	def drop(n: Int): Stream[A] = this match {
		case Cons(_, t) if n > 0 => t().drop(n-1)
		case Cons(_, t) if n == 0 => this
	}

	def takeWhile(p: A => Boolean): Stream[A] = this match {
		case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
		case Empty => Empty
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

	def constatUnfold[A](a: A): Stream[A] =
		unfold(a)(z => Some((a, a+1))) 


	def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
		lazy val head = hd
		lazy val tail = tl
		Cons(() => head, () => tail)
	}

	def empty[A]: Stream[A] = Empty

	def apply[A](as: A*): Stream[A] =
		if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}