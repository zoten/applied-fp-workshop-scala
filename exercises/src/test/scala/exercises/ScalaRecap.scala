package exercises

/*
 * Our most used Scala features are:
 * - Case class
 * - Companion Object
 * - Apply function
 * - Pattern match
 * - Trait as interface
 * - Trait as mixin
 */

class ScalaRecap extends munit.FunSuite {

  /*
   * TODO: One test at a time,
   *       read description
   *       uncomment the code,
   *       and add the code to get a green test
   */
  case class Person(name: String, age: Int) {
    def apply(prefix: String):  String = {
      s"$prefix mi chiamo $name!"
    }

    def makeOlder(amount: Int) = {
      copy(age = age + amount)
    }
    
  }

  object Person {
    def apply(input: String) : Person = {
      create(input)
    }

    def create(input: String) : Person = {
      var a = input.split(";")
      val name = a(0)
      val age = a(1)
      Person(name = name, age = age.toInt)
    }

    def isFake(p : Person) : Boolean = {
      p match {
        case Person("foo", _)          => true
        case Person("bar", _)          => true
        case Person(_, age) if age < 0 => true
        case _                         => false
      }
    }
  }

  test("define case class") {
    // TODO: Define a case class w/ two fields: name and age
    case class Person(name: String, age: Int)
    
    val result = Person("foo", 56)
    assertEquals(result, Person("foo", 56))
  }

  test("define the case class's companion object") {
    // TODO: Define a companion object w/ a creation method that takes one string
    val result = Person.create("foo;56")
    assertEquals(result, Person("foo", 56))
  }

  test("case class apply") {
    // TODO: Define an apply function on Person case class
    val result = Person("foo", 56)("Ciao,")
    assertEquals(result, "Ciao, mi chiamo foo!")
  }

  test("companion object apply") {
    // TODO: Define an apply function on Person companion object
    val result = Person("foo;56")("Ciao,")
    assertEquals(result, "Ciao, mi chiamo foo!")
  }

  test("update case class state") {
    // TODO: Define makeOlder function to increase age
    val p      = Person("foo", 56)
    val result = p.makeOlder(100)
    assertEquals(result.age, 156)
  }
  test("pattern match") {
    // TODO: Define isFake function on Person object that...
    import Person._
    // TODO: ...return true when name is foo
    assert(isFake(Person("foo", 10)))
    // TODO: ...return true when name is bar
    assert(isFake(Person("bar", 10)))
    // TODO: ...return true when age is negative
    assert(isFake(Person("baz", -10)))
    // TODO: ...otherwise return false
    assert(!isFake(Person("baz", 10)))
  }

  trait Fruit {
    def stringify: String
    def eatenBy(name : String) : String = s"$name ate $stringify"
  }

  case class Apple() extends Fruit {
    def stringify : String = "an apple"
  }

  case class Banana() extends Fruit {
    def stringify : String = "a banana"
  }

  test("trait as interface (part 1)") {
    // TODO: Define a Fruit trait w/ two subclass Apple and Banana
    assert(Apple().isInstanceOf[Fruit])
    assert(Banana().isInstanceOf[Fruit])
  }

  test("trait as interface (part 2)") {
    // TODO: Define stringify function on Fruit and implement it in Apple and Banana
    assertEquals(Apple().stringify, "an apple")
    assertEquals(Banana().stringify, "a banana")
  }

  test("trait as mixin") {
    // TODO: Define function w/ implementation on Fruit trait
    assertEquals(Apple().eatenBy("foo"), "foo ate an apple")
    assertEquals(Banana().eatenBy("bar"), "bar ate a banana")
  }
}
