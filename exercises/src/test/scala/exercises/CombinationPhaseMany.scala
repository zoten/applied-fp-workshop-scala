package exercises

// TODO: Remove IgnoreSuite annotation

// @munit.IgnoreSuite
class CombinationPhaseMany extends munit.FunSuite {

  case class Item(name: String, qty: Int)

  def checkName(value: String): Option[String] =
    if (value.nonEmpty) Some(value)
    else None

  def checkQty(qty: String): Option[Int] =
    if (qty.matches("^[0-9]+$")) Some(qty.toInt)
    else None

  // TODO: Implements createItem with for-comprehension

  // NOTE: for-comprehension syntax
  // for {
  //  ... <- ...
  //  ... = ...
  //  ... <- ...
  // } yield ...

  // NOTE: for-comprehension syntax
  // for {
  //  ... <- effectful function (flatMap)
  //  ... = pure (map)
  //  ... <- ...
  // } yield ...

  def createItem(name: String, qty: String): Option[Item] =
    for {
      nm <- checkName(name)
      qt <- checkQty(qty)
    } yield Item(name = nm, qty = qt)

  def checkIn(qty: Int, item: Item): Item =
    item.copy(qty = item.qty + qty)

  def checkOut(qty: Int, item: Item): Option[Item] =
    if (qty <= item.qty) Some(item.copy(qty = item.qty - qty))
    else None

  test("creation") {
    val item = createItem("foo", "100")
    assertEquals(item, Some(Item("foo", 100)))
  }

  test("creation, checkIn and checkOut") {
    // TODO: use for-comprehension to:
    //  - create a 100 "foo" item
    //  - checkIn 10
    //  - checkOut 20
    //  - yield final item
    val result: Option[Item] = for {
      item <- createItem(name = "foo", qty = "100")
      item_checkin = checkIn(qty = 10, item)
      item_checkout <- checkOut(qty = 20, item_checkin)
    } yield item_checkout

    assertEquals(result, Some(Item("foo", 90)))
  }

  test("invalid creation (name)") {
    val item = createItem("", "100")
    assertEquals(item, None)
  }

  test("invalid creation (qty)") {
    val item = createItem("foo", "asd")
    assertEquals(item, None)
  }

  test("invalid creation (both)") {
    val item = createItem("", "asd")
    assertEquals(item, None)
  }
}
