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


//def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]]
//def forAll[A](a: Gen[A])(f: A => Boolean): Prop

//trait Prop { def check: Either[(FailedCase, SuccessCount), SuccessCount] }

import fpinscala.state._


object Prop {
	type FailedCase = String
	type SuccessCount = Int
}

case class Gen[A](sample: State[RNG, A]){
}
object Gen {
	def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
	def boolean: Gen[Boolean] = Gen(State(RNG.nonNegativeInt).map(n => n % 2 == 0))
	def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
		Gen(State.sequence(List.fill(n)(g.sample)))
	def choose(start: Int, stopExclusive: Int): Gen[Int] = 
		Gen(State(RNG.nonNegativeLessThan(stopExclusive - start)).map(n => n + start))
}