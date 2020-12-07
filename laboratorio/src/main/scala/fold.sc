List(1, 2, 3, 4, 5, 6, 7).reduce(_ + _)
List(1, 2, 3, 4, 5, 6, 7).fold(0)(_ + _)

// ES 6
List(List(1, 2, 3), List(4, 5), List(6))
  .reduce(_ ::: _)
Nil.fold(List())(_ ::: _)

// ES 7
val fat: Int => Int = (n) => {
  1.to(n).reduce(_ * _)
}
fat(6)

//Es 8

val somma = (f: Int => Int) => {
  (a: Int, b: Int) => a.to(b).fold(0)(_ + f(_))
  //(a:Int,b:Int) => a.to(b).map(f).reduce(_+_)
}
somma(_ * 2)(5, 7)


//ES 9
val toNumLbe = (l: List[Int]) => {
  l.foldLeft(0, 0)((acc, c) => {
    (acc._1 + (Math.pow(2, acc._2).toInt * c), acc._2 + 1)
  })._1
}
toNumLbe(List(0, 1, 1, 1))

val toNumLle = (l: List[Int]) => {
  l.foldLeft(0)((acc, c) => {
    (acc << 1) + c
  })
}
toNumLle(List(1, 1, 1, 0))

val toNumRbe = (l: List[Int]) => {
  l.foldRight(0)((c, acc) => {
    (acc << 1) + c
  })
}
toNumRbe(List(0, 1, 1, 1))

val toNumRle = (l: List[Int]) => {
  l.foldRight(0, 0)((c, acc) => {
    (acc._1 + (Math.pow(2, acc._2).toInt * c), acc._2 + 1)
  })._1
}
toNumRle(List(1, 1, 1, 0))
//ES 10

val longestMatch: (List[Int], Int => Boolean) => List[Int] = (l, f) => {
  if (l.isEmpty) {
    List()
  } else {
    val l1 = l.takeWhile(f);
    val drop = l.dropWhile(f);
    if (drop.isEmpty) {
      l1
    } else {
      val l2 = longestMatch(l.dropWhile(f).tail, f)
      if (l1.length > l2.length) {
        l1
      } else {
        l2
      }
    }
  }
}
longestMatch(List(1, 2, 3, 4, 1, 2, 2,2,2,3, 3, 4, 1, 1, 1, 1, 1), (_ < 4))




