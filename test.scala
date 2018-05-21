// 8.1
// val intList = Gen.listOf(Gen.choose(0,100))
// val prop_order = forAll(intList)(ns => ns.reverse.sum == ns.sum)
// val sameOntList = Gen.listOf(Gen.choose(1))
// val prop_average = forAll(sameOntList)(ns => ns.sum / ns.length == 1)
//object Gen {
//	def listOf[A](a: Gen[A]): Gen[List[A]]
//	def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]]
//	def forAll[A](a: Gen[A])(f: A => Boolean): Prop
//
//	trait Prop { def check: Boolean }
//}
//trait Prop { def check: Boolean }
//def &&(p: Prop): Prop = new Prop {
//	def check = Prop.this.check && p.check
//}
//case class Gen[A](sample: State[RNG,A])
//object Gen {
//	def choose(start: Int, stopExclusive: Int): Gen[Int] = {
//		Gen( s => )
//	}
//}





import fpinscala.state._
import fpinscala.stream._

//type TestCases = Int
//type Result = Option[(FailedCase, SuccessCount)]
//case class Prop(run: TestCases => Result)

case class Prop(run: (Prop.MaxSize, Prop.TestCases, RNG) => Prop.Result) {
	def &&(p: Prop): Prop = Prop {
		(max, n, rng) => (run(max, n, rng)) match {
			case Prop.Falsified(failure, success) => Prop.Falsified(failure, success)
			case _ => p.run(max, n, rng)
		}
	}

	def ||(p: Prop): Prop = Prop {
		(max, n, rng) => (run(max, n, rng)) match {
			case Prop.Passed => p.run(max, n, rng)
			case x => x
		}
	}
}

object Prop {
	type FailedCase = String
	type SuccessCount = Int
	type TestCases = Int
	type MaxSize = Int

	sealed trait Result {
 	 	def isFalsified: Boolean
 	}

	case object Passed extends Result {
		def isFalsified = false
	}

	case class Falsified(failure: FailedCase,
                       success: SuccessCount) extends Result {
		def isFalsified = true
	}

	def run(p: Prop, maxSize: Int = 100, testCases: Int = 100,
	        rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
		p.run(maxSize, testCases, rng) match {
			case Falsified(msg, n) =>
				println(s"! Falsified after $n passed tests: \n $msg")
			case Passed =>
				println(s"+ Ok, Passed $testCases test.")
		}


	def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
		forAll(g(_))(f)

	def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
		(max, n, rng) => 
			val casesPerSize = (n + (max - 1)) / max
			val props: Stream[Prop] =
				Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
			val prop: Prop =
				props.map(p => Prop { (max, _, rng) =>
					p.run(max, casesPerSize, rng)
				}).toList.reduce(_ && _)
			prop.run(max, n, rng)
	}

	def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
	 	(_max, n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
	 		case (a, i) => try {
	 			if (f(a)) Passed else Falsified(a.toString, i)
	 		} catch { case e: Exception => Falsified(buildMsg(a, e), i)}
	  	}.find(_.isFalsified).getOrElse(Passed)
	}

	def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
		Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

	def buildMsg[A](s: A, e: Exception): String =
		s"tese case: $s\n" +
		s"gnerated an exception: ${e.getMessage}\n" +
		s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}



object Gen {
	def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
	def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
		Gen.boolean.flatMap( b => if (b) g1 else g2)
	def weight[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
		val ratio = g1._2 / (g1._2 + g2._2)
		Gen(State(RNG.double).flatMap(random => if (random < ratio) g1._1.sample else g2._1.sample))
	}

	def boolean: Gen[Boolean] = Gen(State(RNG.nonNegativeInt).map(n => n % 2 == 0))
	def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
		Gen(State.sequence(List.fill(n)(g.sample)))
	def choose(start: Int, stopExclusive: Int): Gen[Int] = 
		Gen(State(RNG.nonNegativeLessThan(stopExclusive - start)).map(n => n + start))
}

case class Gen[+A](sample: State[RNG, A]){
	def flatMap[B](f: A => Gen[B]): Gen[B] = 
		Gen(sample.flatMap(a => f(a).sample))
	def map[B](f: A => B): Gen[B] =
		Gen(sample.map(a => f(a)))
	def listOfN(n: Int): Gen[List[A]] =
		Gen.listOfN(n, Gen(sample))
	def listOfN(size: Gen[Int]): Gen[List[A]] =
		size flatMap(n => this.listOfN(n))
	def unsized: SGen[A] = SGen(_ => this)
}

def ListOf[A](g: Gen[A]): SGen[List[A]] = 
	SGen(n => g.listOfN(n))

def listOf1[A](g: Gen[A]): SGen[List[A]] = 
	SGen(n => g.listOfN(1 max n))

case class SGen[+A](g: Int => Gen[A]) {
	def flatMap[B](f: A => SGen[B]): SGen[B] = 
		SGen(n => g(n) flatMap (a => f(a).g(n)))
	def map[B](f: A => B): SGen[B] =
		SGen(g(_) map (f(_)))
	def apply(n: Int): Gen[A] = g(n)
}

val smallInt = Gen.choose(-10, 10)
val maxProp = Prop.forAll(listOf1(smallInt)) {
	ns => 
		val max = ns.max
		!ns.exists(_ > max)
}
Prop.run(maxProp)

val intList = listOf1(Gen.choose(0,100))
val prop = Prop.forAll(intList)(ns => ns.reverse.reverse == ns) && Prop.forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)
val failingProp = Prop.forAll(intList)(ns => ns.reverse == ns)
Prop.run(prop)
Prop.run(failingProp)



