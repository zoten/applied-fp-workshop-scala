package exercises

// TODO: Remove IgnoreSuite annotation

// @munit.IgnoreSuite
class CreationPhase extends munit.FunSuite {

  case class Item(qty: Int)

  // TODO: Uncomment and complete the type definition for valid or invalid states and
  enum OptionalItem {
    case Some(item: Item)
    case None
  }

  import OptionalItem._

  // TODO: Use OptionalItem as return type
  def createItem(qty: String): OptionalItem =
    if (qty.matches("^[0-9]+$")) Some(Item(qty = qty.toInt))
    else None // typically return null or throw exception

  // scala native
  // def createItemOpt(qty: String): Option[Item] =
  //   if (qty.matches("^[0-9]+$")) Option.Some(Item(qty = qty.toInt))
  //   else Option.None

  test("creation") {
    // TODO: Use OptionalItem as expected type
    assertEquals(createItem("100"), Some(Item(100)))
  }

  test("invalid creation") {
    // TODO: Use OptionalItem as expected type
    assertEquals(createItem("asd"), None)
    assertEquals(createItem("1 0 0"), None)
    assertEquals(createItem(""), None)
  }

}
