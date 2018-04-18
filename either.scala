import scala.{Option => _, Either => _, _}

def Try[A](a: => A): Either[Exception, A] =
  try Right(a)
  catch { case e: Exception => Left(e) }

sealed trait Either[+E, +A] {
	def map[B](f: A => B): Either[E, B] = this match {
		case Left(e) => Left(e)
		case Right(a) => Right(f(a))
	}

	def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
		case Left(e) => Left(e)
		case Right(a) => f(a) 
	}

	def orElse[EE >: E, B >: A](b: Either[EE, B]): Either[EE, B] = this match{
		case Left(_) => b
		case _ => this
	}

	def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
		flatMap (aa => b.map (bb => f(aa, bb)))

	def map2_for[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
		for {
			aa <- this
			bb <- b
		} yield f(aa, bb)
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
	def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
		es match {
			case Nil => Right(Nil)
		    case h :: t => h.flatMap(hh => sequence(t).map(tt => hh :: tt ))
		}
	}

	def sequence_for[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
		es match {
			case Nil => Right(Nil)
		    case h :: t => for {
		    	hh <- h
		    	tt <- sequence(t)
		    } yield (hh :: tt)
		}
	}

	def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
		as match {
			case Nil => Right(Nil)
			case h :: t => f(h).flatMap(hh => traverse(t)(f).map(tt => hh :: tt ))
		}
	}

	def traverse_for[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
		as match {
			case Nil => Right(Nil)
			case h :: t => for {
				hh <- f(h)
				tt <- traverse_for(t)(f)
			} yield (hh :: tt)
		}
	}

	def sequence_better[E, A](es: List[Either[E, A]]): Either[E, List[A]] = 
		traverse(es)(x => x)

}