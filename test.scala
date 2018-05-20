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

//val intList = Gen.listOf(Gen.choose(0,100))
//val prop = forAll(intList)(ns => ns.reverse.reverse == ns) &&
//	       forAll(intLIst)(ns => ns.headOption == ns.reverse.lastOption)
//val failingProp = forAll(intList)(ns => ns.reverse == ns)



import fpinscala.state._

//type TestCases = Int
//type Result = Option[(FailedCase, SuccessCount)]
//case class Prop(run: TestCases => Result)

sealed trait Result {
	def isFalsified: Boolean
}
type SuccessCount = Int
type FailedCase = String

case object Passed extends Result {
	def isFalsified = false
}
case class isFalsified(failure: FailedCase,
                       success: SuccessCount) extends Result {
	def isFalsified = true
}

object Prop {
	type FailedCase = String
	type SuccessCount = Int
}

case class Gen[A](sample: State[RNG, A]){
	def flatMap[B](f: A => Gen[B]): Gen[B] = 
		Gen(sample.flatMap(a => f(a).sample))
	def listOfN(n: Int): Gen[List[A]] =
		Gen.listOfN(n, Gen(sample))
	def listOfN(size: Gen[Int]): Gen[List[A]] =
		size flatMap(n => this.listOfN(n))
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