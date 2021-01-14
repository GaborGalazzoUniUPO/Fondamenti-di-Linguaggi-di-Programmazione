import java.io.{FileOutputStream, OutputStreamWriter, PrintStream, PrintWriter, Writer}
// ES 19



class Conto (sIn: Double) {
  protected var saldo = sIn
  def preleva (x: Double)={
    if (x<=saldo) {saldo -= x;Some(x)}
    else None
  }
  def versa (x: Double): Unit={
    saldo += x
  }
  def saldoAttuale() = {
    saldo
  }
}

trait Logger {
 protected def log(msg: String):Unit // Un metodo astratto
}

trait StreamLogger extends Logger {
  val writer: PrintStream
  protected def log(msg: String):Unit = {
    writer.println(msg)
  }
}

trait ConsoleLogger extends StreamLogger {
  val writer = System.out
  override protected def log(msg: String):Unit = { println(msg) }
}

trait FileLogger extends StreamLogger {
  val filename: String
  lazy val writer = new PrintStream(new FileOutputStream(filename)) // Parte del costruttore
  override protected def log(msg: String):Unit = {
    writer.println(msg);
    writer.flush()
  }
}

trait TimestampLogger extends StreamLogger {
  override def log(msg: String):Unit ={
    super.log(s"${java.time.Instant.now()} $msg")
  }
}

trait ShortLogger extends StreamLogger {
  protected override def log(msg: String):Unit = {
    super.log(if (msg.length <= 15) msg else s"${msg.substring(0, 12)}...")
  }
}


trait CesarLogger extends StreamLogger {
  protected override def log(msg: String):Unit ={
    super.log(msg.map(_ + 3).map(_.asInstanceOf[Char]).foldLeft("")((s:String,c) => s+c))
  }
}


abstract class ContoConLog(sIn: Double) extends Conto(sIn) with Logger {
  override def preleva(x: Double) = {
    log(s"Prelievo di ${x}")
    super.preleva(x)
  }

  override def versa(x: Double): Unit = {
    log(s"Versamento di ${x}")
    super.versa(x)
  }

  override def saldoAttuale() ={
    log(s"Richiesta di saldo")
    super.saldoAttuale()
  }
}
val doThings = (c: Conto)  => {
  c.versa(5d)
  c.preleva(4d)
  c.saldoAttuale()
}
doThings(new ContoConLog(10.0) with ConsoleLogger)
doThings(new ContoConLog(10.0) with ConsoleLogger with ShortLogger)
doThings(new ContoConLog(10.0) with ConsoleLogger with CesarLogger)
doThings(new ContoConLog(10.0) with ConsoleLogger with CesarLogger with ShortLogger)
doThings(new ContoConLog(10.0) with ConsoleLogger with ShortLogger with CesarLogger)
doThings(new ContoConLog(10.0) with FileLogger {
  override val filename = "/home/20024195/workspace/Fondamenti-di-Linguaggi-di-Programmazione/laboratorio/log.log"
})
