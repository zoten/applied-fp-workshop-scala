package application

class Version1Tests extends munit.FunSuite {

  import application.Version1._

  // TODO: implements tests

  test("turn right command") {
    val planet = Planet(Size(5, 4), List())
    val rover = Rover(Coordinate(0, 0), Rotation.North)
    val command = Command.TurnRight
    val result = execute(planet, rover, command)
    assertEquals(result, Rover(Coordinate(0, 0), Rotation.East))
  }

  test("turn left command") {
    val planet = Planet(Size(5, 4), List())
    val rover = Rover(Coordinate(0, 0), Rotation.North)
    val command = Command.TurnLeft
    val result = execute(planet, rover, command)
    assertEquals(result, Rover(Coordinate(0, 0), Rotation.West))
  }

  test("move forward command") {
    val planet = Planet(Size(5, 4), List())
    val rover = Rover(Coordinate(0, 1), Rotation.North)
    val command = Command.Forward
    val result = execute(planet, rover, command)

    assertEquals(result, Rover(Coordinate(0, 2), Rotation.North))
  }

  test("move forward command, opposite orientation") {
    val planet = Planet(Size(5, 4), List())
    val rover = Rover(Coordinate(0, 1), Rotation.South)
    val command = Command.Forward
    val result = execute(planet, rover, command)

    assertEquals(result, Rover(Coordinate(0, 0), Rotation.South))
  }

  test("move backward command") {
    val planet = Planet(Size(5, 4), List())
    val rover = Rover(Coordinate(0, 1), Rotation.North)
    val command = Command.Backwards
    val result = execute(planet, rover, command)
    assertEquals(result, Rover(Coordinate(0, 0), Rotation.North))
  }

  test("move backward command, opposite orientation") {
    val planet = Planet(Size(5, 4), List())
    val rover = Rover(Coordinate(0, 1), Rotation.South)
    val command = Command.Backwards
    val result = execute(planet, rover, command)
    assertEquals(result, Rover(Coordinate(0, 2), Rotation.South))
  }

  test("wrap on North") {
    val planet = Planet(Size(5, 4), List())
    val rover = Rover(Coordinate(0, 3), Rotation.North)
    val command = Command.Forward
    val result = execute(planet, rover, command)
    assertEquals(result, Rover(Coordinate(0, 0), Rotation.North))
  }

  test("go to opposite angle") {
    val planet = Planet(Size(5, 4), List())
    val rover = Rover(Coordinate(0, 0), Rotation.North)
    val commands = List(Command.TurnLeft, Command.Forward, Command.TurnRight, Command.Backwards)
    val result = executeAll(planet, rover, commands)
    assertEquals(result, Rover(Coordinate(4, 3), Rotation.North))
  }
}
