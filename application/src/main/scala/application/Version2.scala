package application

/*
    ## V2 - Focus on boundaries (from primitive to domain types and viceversa)

    Our domain is declared with rich types but inputs/outputs are should be primitive types

    - Write a parser for input planet data (size, obstacles)
    - Write a parser for input rover data (position, orientation)
    - Write a parser for input commands (skip unknown chars)
    - Render the final result as string: "positionX:positionY:orientation"
 */
object Version2 {

  import Orientation._, Command._, ParseError._
  import cats.syntax.either._
  import cats.syntax.traverse._

  // TODO 11:
  //  - combine planet, rover and commands parsers
  //  - call executeAll
  //  - render the mission result
  def runMission(inputPlanet: (String, String), inputRover: (String, String), inputCommands: String): Either[ParseError, String] =
    for {
      planet <- parsePlanet(inputPlanet)
      rover <- parseRover(inputRover)
      commands = parseCommands(inputCommands)
      result = executeAll(planet, rover, commands)
      rendered = render(result)
    } yield rendered

  // PARSING

  // TODO 1: implements (input = 'F')
  def parseCommand(input: Char): Command = input.toString().toUpperCase() match {
    case "F" => MoveForward
    case "B" => MoveBackward
    case "R" => TurnRight
    case "L" => TurnLeft
    case _   => Unknown
  }

  // TODO 2: combine parseCommand (input = "BFFLLR")
  // in Scala () are used in function signature to warn there may be side effects
  // why input.toCharArray().toList().map(parseCommand) doesn't work?
  // input.toCharArray.toList.map(parseCommand)
  def parseCommands(input: String): List[Command] =
    input.toList.map(parseCommand)

  // TODO 3: combine parseIntTuple (input = "3,2"), error should be InvalidRover
  def parsePosition(input: String): Either[ParseError, Position] =
    parseIntTuple(",", input)
      .leftMap(_ => ParseError.InvalidRover(message = s"invalid position: ${input}"))
      .map((x, y) => Position(x = x, y = y))

  // TODO 4: implements (input = "N"), error should be InvalidRover
  def parseOrientation(input: String): Either[ParseError, Orientation] = input.toString().toUpperCase() match {
    case "N" => Right(N)
    case "S" => Right(S)
    case "W" => Right(W)
    case "E" => Right(E)
    case _   => Left(ParseError.InvalidRover(message = s"invalid orientation: ${input}"))
  }

  // TODO 5: combine parsePosition and parseOrientation, error should be InvalidRover
  def parseRover(input: (String, String)): Either[ParseError, Rover] = {
    val (position, orientation) = input
    for {
      parsed_position <- parsePosition(position)
      parsed_orientation <- parseOrientation(orientation)
    } yield Rover(position = parsed_position, orientation = parsed_orientation)
  }

  // TODO 6: combine parseIntTuple (input = "5x5"), error should be InvalidPlanet
  def parseSize(input: String): Either[ParseError, Size] = parseIntTuple("x", input)
    .leftMap(_ => ParseError.InvalidPlanet(message = s"invalid size: ${input}"))
    .map((w, h) => Size(width = w, height = h))

  // TODO 7: combine parsePosition (input = "3,2"), error should be InvalidPlanet
  def parseObstacle(input: String): Either[ParseError, Obstacle] =
    parsePosition(input)
      .map(p => Obstacle(p))
      .leftMap(_ => ParseError.InvalidPlanet(message = s"invalid obstacle: ${input}"))

    // other formulation
    // parseIntTuple(",", input)
    // .leftMap(_ => ParseError.InvalidPlanet(message = s"invalid obstacle: ${input}"))
    // .map((x, y) => Obstacle(Position(x = x, y = y)))

  // TODO 8: combine parseObstacle (input = "3,2 7,0 2,9"), error should be InvalidPlanet
  def parseObstacles(input: String): Either[ParseError, List[Obstacle]] =
    input.split(" ").toList.traverse(parseObstacle)

  // TODO 9: combine parseSize and parseObstacles, error should be InvalidPlanet
  def parsePlanet(input: (String, String)): Either[ParseError, Planet] = {
    val (size, obstacles) = input
    for {
      parsed_size <- parseSize(size)
      parsed_obstacles <- parseObstacles(obstacles)
    } yield Planet(size = parsed_size, obstacles = parsed_obstacles)
  }

  // NOTE: utility function to split a string in a pair of ints
  def parseIntTuple(separator: String, input: String): Either[Throwable, (Int, Int)] =
    Either.catchNonFatal {
      val parts = input.split(separator).toList
      (parts(0).trim.toInt, parts(1).trim.toInt)
    }

  // RENDERING

  // TODO 10: implements render, combine the mission result string
  def render(rover: Rover): String = s"${rover.position.x}:${rover.position.y}:${rover.orientation.toString}"

  // DOMAIN
  def executeAll(planet: Planet, rover: Rover, commands: List[Command]): Rover =
    commands.foldLeft(rover)((prev, cmd) => execute(planet, prev, cmd))

  def execute(planet: Planet, rover: Rover, command: Command): Rover =
    command match {
      case TurnRight    => turnRight(rover)
      case TurnLeft     => turnLeft(rover)
      case MoveForward  => moveForward(planet, rover)
      case MoveBackward => moveBackward(planet, rover)
      case Unknown      => rover
    }

  def turnRight(rover: Rover): Rover =
    rover.copy(orientation = rover.orientation match {
      case N => E
      case E => S
      case S => W
      case W => N
    })

  def turnLeft(rover: Rover): Rover =
    rover.copy(orientation = rover.orientation match {
      case N => W
      case W => S
      case S => E
      case E => N
    })

  def moveForward(planet: Planet, rover: Rover): Rover =
    rover.copy(position = next(planet, rover, delta(rover.orientation)))

  def moveBackward(planet: Planet, rover: Rover): Rover =
    rover.copy(position = next(planet, rover, delta(opposite(rover.orientation))))

  def next(planet: Planet, rover: Rover, delta: Delta): Position = {
    val position = rover.position
    position.copy(
      x = wrap(position.x, planet.size.width, delta.x),
      y = wrap(position.y, planet.size.height, delta.y)
    )
  }

  def opposite(orientation: Orientation): Orientation =
    orientation match {
      case N => S
      case S => N
      case E => W
      case W => E
    }

  def delta(orientation: Orientation): Delta =
    orientation match {
      case N => Delta(0, 1)
      case S => Delta(0, -1)
      case E => Delta(1, 0)
      case W => Delta(-1, 0)
    }

  def wrap(value: Int, limit: Int, delta: Int): Int =
    (((value + delta) % limit) + limit) % limit

  // TYPES
  case class Delta(x: Int, y: Int)
  case class Position(x: Int, y: Int)
  case class Size(width: Int, height: Int)
  case class Obstacle(position: Position)
  case class Planet(size: Size, obstacles: List[Obstacle])
  case class Rover(position: Position, orientation: Orientation)

  enum ParseError {
    case InvalidPlanet(message: String)
    case InvalidRover(message: String)
  }

  enum Command {
    case MoveForward
    case MoveBackward
    case TurnRight
    case TurnLeft
    case Unknown
  }

  enum Orientation {
    case N, E, W, S
  }

}
