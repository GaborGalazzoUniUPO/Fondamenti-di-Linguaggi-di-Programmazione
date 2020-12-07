import scala.annotation.tailrec

// ES1 Fibonacci Tail Recursion
val fib: Int => BigInt = (n: Int) => {
  @tailrec
  def aFib(c: Int, n: Int, m: Int): BigInt = {
    if (c < 1) m
    else aFib(c - 1, m, n + m)
  }

  aFib(n, 0, 1)
}

fib(100000000)

//ES2 Length Tail Recursion
val len: (List[Int] => Int) = (l) => {
  @tailrec
  def aLen(l: List[Int], acc: Int): Int = {
    l match {
      case Nil => acc
      case _ :: tl => aLen(tl, acc + 1)
    }
  }

  aLen(l, 0)
}

val x = List.range(0, 100)
len(x)


//ES3 Vocali
val caratteri: List[Char] => (Int, Int, Int, Int, Int) = (list) => {
  @tailrec
  def caratteriRC(l: List[Char], a: Int, e: Int, i: Int, o: Int, u: Int): (Int, Int, Int, Int, Int) = {
    l match {
      case Nil => (a, e, i, o, u)
      case 'a' :: lt => caratteriRC(lt, a + 1, e, i, o, u)
      case 'e' :: lt => caratteriRC(lt, a, e + 1, i, o, u)
      case 'i' :: lt => caratteriRC(lt, a, e, i + 1, o, u)
      case 'o' :: lt => caratteriRC(lt, a, e, i, o + 1, u)
      case 'u' :: lt => caratteriRC(lt, a, e, i, o, u + 1)
      case _ :: lt => caratteriRC(lt, a, e, i, o, u)

    }
  }

  caratteriRC(list, 0, 0, 0, 0, 0)
}

caratteri(List('a', 'b', 'e', 'i', 'a', 'c'))

//ES4 Vocali

val flat1: (List[List[Int]] => List[Int]) = {
  case Nil => Nil
  case h :: t => h match {
    case h1 :: Nil => h1 :: flat1(t)
    case h1 :: t1 => h1 :: flat1(t1 :: t)
  }
}
flat1(List(List(1, 2, 3), List(4, 5), List(6)))

val flat2: (List[List[Int]] => List[Int]) = (l) => {
  @tailrec
  def flat2RC(in: List[List[Int]], acc: List[Int]): List[Int] = {
    in match {
      case Nil => acc.reverse
      case h :: t => h match {
        case h1 :: Nil => flat2RC(t, h1 :: acc)
        case h1 :: t1 => flat2RC(t1 :: t, h1 :: acc)
      }
    }
  }
  flat2RC(l, List())
}
flat2(List(List(1, 2, 3), List(4, 5), List(6)))

val flatFacile: (List[List[Int]] => List[Int]) = (l) => {
  @tailrec
  def flatFacileRC(in: List[List[Int]], acc: List[Int]): List[Int] = {
    in match {
      case Nil => acc
      case h :: lt => flatFacileRC(lt, acc ::: h)
    }
  }
  flatFacileRC(l, List())
}

flatFacile(List(List(1, 2, 3), List(4, 5), List(6)))

//ES5
val insertOrd: (List[Int], Int) => List[Int] = (l, i) => {
  l.takeWhile(_ < i).appendedAll(i :: l.dropWhile(_ >= i))
}