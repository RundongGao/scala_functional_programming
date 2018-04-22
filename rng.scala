trait RNG {
	def nextInt: (Int, RNG)
}
case class SimpleRNG(seed: Long) extends RNG {
	def nextInt: (Int, RNG) = {
		val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
		val nextRNG = SimpleRNG(newSeed)
		val n = (newSeed >>> 16).toInt
		(n, nextRNG)
	}
}
type Rand[+A] = RNG => (A, RNG)

object RNG {
	def nonNegativeInt(rng: RNG): (Int, RNG) = {
		val (n, newRng) = rng.nextInt
	    (if (n < 0) -n+1 else n, newRng) 
	}

    def double(rng: RNG): (Double, RNG) = {
    	val (n, newRng) = nonNegativeInt(rng)
    	(n.toDouble / Int.MaxValue, newRng)
    }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
        val (i, r1) = rng.nextInt
        val (d, r2) = double(r1)
        ((i,d), r2)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
        val ((i, d), r) = intDouble(rng)
        ((d,i), r)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    	val (d1, r1) = double(rng)
    	val (d2, r2) = double(r1)
    	val (d3, r3) = double(r2)
    	((d1,d2,d3), r3)
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    	if (count == 0)
    		(List(), rng)
    	else {
	    	val (i, r) = rng.nextInt
	    	val (ls, r2) = ints(count-1)(r)
	    	(i :: ls, r2)
	    }
    }

    def unit[A](a: A): Rand[A] =
      rng => (a, rng)

    def map[A, B](s: Rand[A])(f: A=>B): Rand[B] = 
    	rng => {
    		val (a, rng2) = s(rng)
    		(f(a), rng2)
    	}
    def nonNegativeEven: Rand[Int] =
    	map(nonNegativeInt)(i => i - i % 2)

    def doubleWithMap: Rand[Double] =
    	map(nonNegativeInt)(i => i.toDouble / Int.MaxValue )

    def int: Rand[Int] = _.nextInt

    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C) : Rand[C] = 
    	rng => {
    		val (a, r1) = ra(rng)
    		val (b, r2) = rb(r1)
    		(f(a,b), r2)

    	}

    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    	map2(ra, rb)((_, _))

    val randIntDouble: Rand[(Int, Double)] = 
    	both(int, double)

    val randDoubleInt: Rand[(Double, Int)] = 
    	both(double, int)

    // @_@!
    //def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    //    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

    def flatMap[A, B](f: Rand[A])(g : A => Rand[B]): Rand[B] = {
    	rng => {
    		val (a, rng2) = f(rng)
            g(a)(rng2)
    	}
    }

    def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    	val (i, rng2) = nonNegativeInt(rng)
    	val mod = i % n
    	if (i + (n-1) - mod >= 0)
    		(mod, rng2)
        else nonNegativeLessThan(n)(rng)
    }

    def nonNegativeLessThanFlatMap(n: Int): Rand[Int] =
    	flatMap(nonNegativeInt)(i => {
    		val mod = i % n
    		if (i + (n-1) - mod >= 0) unit(mod)
    	    else  nonNegativeLessThanFlatMap(n)
    })

    def mapFlatMap[A, B](s: Rand[A])(f: A=>B): Rand[B] =
    	flatMap(s)(x => unit(f(x)))

    def map2FlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C) : Rand[C] =
    	flatMap(ra)(a => map(rb)(b => f(a,b)))
}
