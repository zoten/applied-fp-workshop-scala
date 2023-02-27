package application

// TODO: Remove IgnoreSuite annotation

// @munit.IgnoreSuite
class Version2Tests extends munit.FunSuite {

  import application.Version2._
  import application.Version2.Orientation._, Command._, ParseError._

  test("go to opposite angle") {
    val planet = ("5x4", "2,0 0,3 3,2")
    val rover = ("0,0", "N")
    val commands = "RBBLBRF"

    val result = runMission(planet, rover, commands)

    assertEquals(result, Right("4:3:E"))
  }

  test("invalid planet input data") {
    val planet = ("ax4", "2,0 0,3 3,2")
    val rover = ("1,2", "N")
    val commands = "RBRF"

    val result = runMission(planet, rover, commands)

    assertEquals(result, Left(InvalidPlanet("invalid size: ax4")))
  }

  test("invalid rover input data") {
    val planet = ("5x4", "2,0 0,3 3,2")
    val rover = ("1,2", "X")
    val commands = "RBRF"

    val result = runMission(planet, rover, commands)

    assertEquals(result, Left(InvalidRover("invalid orientation: X")))
  }

  test("unknown command") {
    val planet = ("5x4", "2,0 0,3 3,2")
    val rover = ("1,2", "N")
    val commands = "RBXRF"

    val result = runMission(planet, rover, commands)

    assertEquals(result, Right("0:1:S"))
  }
}
