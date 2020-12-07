// Fibonacci Tail Recursion
val fib: Int => BigInt = (n: Int) => {
  def aFib(c: Int, n: Int, m: Int): BigInt = {
    if (c < 1) m
    else aFib(c - 1, m, n + m)
  }

  aFib(n, 0, 1)
}

fib(100000000)

//Length Tail Recursion
/*
val lunghezza: (List[Int] => Int) = (l) => l match {
  case Nil => 0
  case _ :: tl => 1 + lunghezza(tl)
}*/
val len: (List[Int] => Int) = (l) => {
  def aLen(l:List[Int],acc:Int): Int ={
    l match {
      case Nil => acc
      case _ :: tl => aLen(tl, acc+1)
    }
  }
  aLen(l,0)
}
val x = List.range(0, 100)
len(x)

