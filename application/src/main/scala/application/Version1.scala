package application

/*
    ## V1 - Focus on the center (pure domain logic)

    Develop an API (types and functions) that executes commands:

    - Implement all commands logic.
    - Commands are sent in batch and executed sequentially.
    - The planet grid has a wrapping effect from one edge to another (pacman).
    - For now, ignore obstacle detection logic
 */
object Version1 {

  // TODO 1: Those type alias are only placeholders,
  //  use correct type definitions and feel free to add more...
  // type Rover = String
  // type Planet = String
  // type Command = String

  // type Obstacle = Position
  case class Obstacle(position: Coordinate)

  case class Rover(position: Coordinate, rotation: Rotation)

  case class Size(width: Int, height: Int)

  case class Planet(size: Size, obstacles: List[Obstacle])

  enum Command {
    case Forward
    case Backwards
    case TurnRight
    case TurnLeft
  }

  case class Coordinate(x: Int, y: Int)

  // rover's orientation
  enum Rotation {
    case North
    case South
    case West
    case East

  }

  // TODO 2: Execute all commands and accumulate final rover state
  def executeAll(planet: Planet, rover: Rover, commands: List[Command]): Rover = {
    commands.foldLeft(rover)(
      (acc, command) =>
        execute(planet, acc, command)
    )
  }

  // TODO 3: Dispatch one command to a specific function
  def execute(planet: Planet, rover: Rover, command: Command): Rover = {
    command match {
      case Command.Forward => moveForward(planet, rover)
      case Command.Backwards => moveBackward(planet, rover)
      case Command.TurnRight => turnRight(rover)
      case Command.TurnLeft => turnLeft(rover)
    }
  }

  // TODO 4: Change rover orientation
  def turnRight(rover: Rover): Rover = {
    val new_rotation = rover.rotation match {
      case Rotation.North => Rotation.East
      case Rotation.East => Rotation.South
      case Rotation.South => Rotation.West
      case Rotation.West => Rotation.North
    }
    rover.copy(rotation = new_rotation)
  }

  // TODO 5: Change rover orientation
  def turnLeft(rover: Rover): Rover = {
    val new_rotation = rover.rotation match {
      case Rotation.North => Rotation.West
      case Rotation.West => Rotation.South
      case Rotation.South => Rotation.East
      case Rotation.East => Rotation.North
    }
    rover.copy(rotation = new_rotation)
  }

  // TODO 6: Change rover position
  def moveForward(planet: Planet, rover: Rover): Rover = {
    val new_position = rover.rotation match {
      case Rotation.North => 
        rover.position.copy(
          y = wrap(rover.position.y, planet.size.height, 1)
        )
      case Rotation.South => rover.position.copy(
          y = wrap(rover.position.y, planet.size.height, -1)
        )
      case Rotation.West => 
        rover.position.copy(
          x = wrap(rover.position.x, planet.size.width, -1)
        )
      case Rotation.East =>
        rover.position.copy(
          x = wrap(rover.position.x, planet.size.width, 1)
        )
    }
    
    rover.copy(position = new_position)
  }

  // TODO 7: Change rover position
  def moveBackward(planet: Planet, rover: Rover): Rover = {
    val new_position = rover.rotation match {
      case Rotation.North => 
        rover.position.copy(
          y = wrap(rover.position.y, planet.size.height, -1)
        )
      case Rotation.South => rover.position.copy(
          y = wrap(rover.position.y, planet.size.height, 1)
        )
      case Rotation.West => 
        rover.position.copy(
          x = wrap(rover.position.x, planet.size.width, 1)
        )
      case Rotation.East =>
        rover.position.copy(
          x = wrap(rover.position.x, planet.size.width, - 1)
        )
    }
    
    rover.copy(position = new_position)
  }


  // NOTE: utility function to get the pacman effect
  def wrap(value: Int, limit: Int, delta: Int): Int =
    (((value + delta) % limit) + limit) % limit
}
