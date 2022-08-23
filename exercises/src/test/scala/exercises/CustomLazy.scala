package exercises

class CustomLazy extends munit.FunSuite {

  // TODO: remove ignores

  case class Lazy[A](func: () => A) {

    // TODO: implement map function
    def map[B](f: A => B): Lazy[B] = ???

    // TODO: implement flatMap function
    def flatMap[B](f: A => Lazy[B]): Lazy[B] = ???

    // TODO: implement fold function
    def fold(): A = ???
  }

  object Lazy {
    // TODO: implement pure function
    def pure[A](a: () => A): Lazy[A] = ???
  }

  def expensiveComputation(): Int = {
    log("expensive")
    42
  }

  def increment(x: Int): Int = {
    log("increment")
    x + 1
  }

  def reverseString(x: Int): Lazy[String] = {
    log("reversed")
    Lazy.pure(() => x.toString.reverse)
  }

  test("creation phase".ignore) {
    val result = captureOutput {
      Lazy
        .pure(expensiveComputation)
    }

    assertEquals(result, List())
  }

  test("combination phase - normal".ignore) {
    val result = captureOutput {
      Lazy
        .pure(expensiveComputation)
        .map(increment)
    }

    assertEquals(result, List())
  }

  test("combination phase - effectful".ignore) {
    val result = captureOutput {
      Lazy
        .pure(expensiveComputation)
        .flatMap(reverseString)
    }

    assertEquals(result, List())
  }

  test("removal phase value".ignore) {
    val result = captureOutput {
      Lazy
        .pure(expensiveComputation)
        .map(increment)
        .flatMap(reverseString)
        .fold()
    }

    assertEquals(result, List("expensive", "increment", "reversed"))
  }

  def log(message: String): Unit =
    System.out.println(message)

  def captureOutput[A](action: => Unit): List[String] = {
    import java.io.{ ByteArrayInputStream, ByteArrayOutputStream, PrintStream }

    val originalOut = System.out
    try {
      val out = new ByteArrayOutputStream
      System.setOut(new PrintStream(out))

      action

      if out.toString.isEmpty
      then List()
      else out.toString.replace("\r", "").split('\n').toList
    } finally System.setOut(originalOut)
  }
}