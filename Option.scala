
import scala.{Option => _, Either => _, _}

def Try[A](a: => A): Option[A] =
  try Some(a)
  catch { case e: Exception => None }

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  def orElse2[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = {
    flatMap((x) => if (f(x)) Some(x) else None)
  }
}  
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap ((m) =>  mean(xs.map ((x) => math.pow(x-m,2))))
  }

  def map2_bad[A, B ,C](a: Option[A], b: Option[B])(f: (A, B)=> C): Option[C] = {
    (a, b) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(a), Some(b)) =>  Some(f(a,b))
    }
  }
  def map2[A, B ,C](a: Option[A], b: Option[B])(f: (A, B)=> C): Option[C] = 
    a flatMap ((aa) => b map((bb) => f(aa,bb)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t).map (tt => hh :: tt))
    }
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      //case h :: t => h.flatMap (hh => hh :: traverse(t)(f)) 
      case h :: t => f(h) flatMap (hh => traverse(t)(f).map(tt => hh :: tt))
    }
  }

  def sequence_via_traverse[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(x => x)
  }

}

println("shoulde be 1.5 ", Option.mean(Seq(1.0, 2.0)))
println("shoulde be 0.25 ", Option.variance(Seq(1.0, 2.0)))
println("shoulde be None ", 1)
println(Option.map2(Some(1), Some(2))((x,y) => x+y))

