class Main {
  def main(args: Array[String]): Unit = {
    val last:(List[Int]=>List[Int]) = {
      case n :: Nil => List(n)
      case _ :: t1 => last(t1)
    }
    last(List(1,2,5))
  }

}
