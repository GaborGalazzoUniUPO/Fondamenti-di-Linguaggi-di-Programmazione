// ES 18

class Stack[+T] protected(protected val stack: List[T]){
       def top(): T = {
              stack.head
       }
       def push[E >: T](el: E): Stack[E] = {
              new Stack[E](el::stack)
       }
       def pop(): Stack[T] = {
              new Stack[T](stack.tail)
       }
}
object Stack{
       val empty = new Stack[Nothing](Nil)
}
val stack = Stack.empty
val s2: Stack[String] = stack.push("ciao")
val s2_t: Stack[Any] = s2;
val s3 = s2_t.push(new Object())
val s4 = s3.push(2)
val s5 = s4.pop()





