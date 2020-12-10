import java.util
import java.util.Scanner
import scala.io.Source

// ES 11
val paola =("Paola", true,Nil)
val andrea =("Andrea", true,List(paola))
val peter =("Peter", false,Nil)
val giulia =("Giulia", true, List(paola, peter))
val persone = List(paola, peter, giulia)

val nameStartWithP = for((nome,_,_) <- persone; if (nome(0) == 'p' || nome(0) == 'P')  ) yield nome

val motherSon = for((nome,isFemmina,figli) <- persone if isFemmina;
                    (figlio,_,_) <- figli) yield (nome,figlio)

//ES 12
val removeNext: List[Int] => List[Int] = (l) => {
  for((a,b) <- l.zip(l.tail)) yield a-b
}

removeNext(  List(3,2,7,4,4) )


//ES 13

val countWords: String => List[Tuple2[String,Int]] = (filename) => {
  Source.fromFile(filename)
    .getLines()
    .flatMap(_.split(Array(' ',';',',')))
    .foldLeft(Map.empty[String, Int]){
      (count, word) => count + (word -> (count.getOrElse(word, 0) + 1))
    }
    .toList
    .sortBy(_._1)
}
countWords("/home/20024195/workspace/Fondamenti-di-Linguaggi-di-Programmazione/laboratorio/words.txt")

//ES 14
val take: LazyList[Int]=>Int=>LazyList[Int] = (l) => {
  (n) => {
    if(n == 0) LazyList.empty
    else
    l match {
      case LazyList() => LazyList()
      case h#::t => h#::take(t)(n-1)
    }
  }
}

take(1#::2#::LazyList())(4).toList
take(1#::1#::1#::1#::1#::1#::LazyList())(4).toList
take(1#::1#::1#::1#::LazyList())(4).toList

//ES 15
val sequenza: List[Option[Int]] => Option[List[Int]] = l => {
 def seqA(acc:List[Int],l: List[Option[Int]]): Option[List[Int]] ={
   l match {
     case Nil => Some(acc)
     case Some(h)::t => seqA(h::acc,t)
     case None :: _ => None
   }
 }
  seqA(List(),l)
}

val seqIter:  List[Option[Int]] => Option[List[Int]] = l => {
  var resp: Option[List[Int]] = Some(List[Int]())
  var s = l;
  while(s.nonEmpty){
    if(s.head.isDefined){
      resp = Some(s.head.get::resp.get)
      s = s.tail
    }else{
      s = Nil
      resp = None
    }
  }
  resp
}

sequenza (List(Some(1),Some(2),Some(77)))
sequenza (List(Some(1),None,Some(77)))

seqIter (List(Some(1),Some(2),Some(77)))
seqIter (List(Some(1),None,Some(77)))

//ES 15.b

val tentaVal:(=> Int)=>Option[Int] =(a)=>
  try Some(a)
  catch { case e: Exception => None }

val parseInts:List[String]=>Option[List[Int]] = l => {
  sequenza(l.map(s => tentaVal(s.toInt)))
}

parseInts (List("1","2","77"))
parseInts (List("1","due","77"))
