import java.util.concurrent._
import language.implicitConversions

type Par[A] = ExecutorService => Future[A]

object Par {
	def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
	private case class UnitFuture[A](get: A) extends Future[A] {
		def isDone= true
		def get(timeout: Long, units: TimeUnit) = get
		def isCancelled = false
		def cancel(evenIfRunning: Boolean): Boolean = false
	}

	def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

	def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = 
		(es: ExecutorService) => {
			val af = a(es)
			var bf = b(es)
			UnitFuture(f(af.get, bf.get))
		}

	def fork[A](a: => Par[A]): Par[A] =
		es => es.submit(new Callable[A] {
			def call = a(es).get
		})

	def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

	def asyncF[A, B](f:A=>B): A=>Par[B] = a => lazyUnit(f(a))

	def map[A, B](a: Par[A])(f: A => B): Par[B] =
		map2(a, unit(()))((a, _) => f(a))

	def sequence[A](ps: List[Par[A]]): Par[List[A]] =
		ps.foldRight(unit(List[A]()))((p, acc) => map2(p, acc)(_ :: _))

	def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
        val fbs: List[Par[B]] = ps.map(asyncF(f))
		sequence(fbs)
    }

	def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
		var pars: List[Par[List[A]]] =
			as.map(asyncF((a: A) => if (f(a)) List(a) else List()))
	    map(sequence(pars))(_.flatten)
	}

	def chocieN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
		es => {
          var index = run(es)(n).get
		  choices(index)(es)
		}

	def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
		es => {
			var index =  run(es)(key).get
			choices(index)(es)
		}

	def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = 
		es => {
			var key =  run(es)(pa).get
			choices(key)(es)
		}

	def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    	chooser(cond)(x => if (x) t else f)

    def choiceNViaChooserchocieN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    	chooser(n)(choices)

    def faltMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    	es => {
			var aa =  run(es)(a).get
			f(aa)(es)
		}

    def join[A](a: Par[Par[A]]): Par[A] =
    	faltMap(a)(x => x)




}