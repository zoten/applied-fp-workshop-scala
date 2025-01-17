package application

/*
    ## V6 (bonus iteration) - Testability via values (Elm-ish architectural style)

    Use values to obtain a strong and simple separation between domain and infrastructure logic

    - implement init, update and test them without infrastructure and mocks
    - implement infrastructure and test it with integration tests
 */
object Version6 {

  import application.Console._
  import application.File._
  import Orientation._, Command._, ParseError._
  import cats.syntax.either._
  import cats.syntax.traverse._
  import cats.syntax.applicativeError._
  import cats.effect.IO

  // NOTE: Domain and Infrastructure "talks" to each other with values passed through the runtime

  // NOTE: Events represent something that happened in the infrastructure (Infrastructure => Domain)
  enum Event {
    case LoadMissionSuccessful(planet: Planet, rover: Rover)
    case LoadMissionFailed(error: Throwable)
    case CommandsReceived(commands: List[Command])
  }

  // NOTE: Effects represent something the Infrastructure has to do (Domain => Infrastructure)
  enum Effect {
    case LoadMission(planetFile: String, roverFile: String)
    case AskCommands
    case ReportObstacleDetected(rover: ObstacleDetected)
    case ReportCommandSequenceCompleted(rover: Rover)
    case ReportKo(error: Throwable)
  }

  // NOTE: All possible discrete states of the Domain
  enum AppState {
    case Loading
    case Ready(planet: Planet, rover: Rover)
    case Failed
  }

  // TODO 1: Initially we are in the Loading state and we want to load mission
  def init(planetFile: String, roverFile: String): (AppState, Effect) = ???

  def update(model: AppState, event: Event): (AppState, Effect) =
    (model, event) match {

      // TODO 2: change state and ask for commands
      case (AppState.Loading, Event.LoadMissionSuccessful(planet, rover)) =>
        ???

      // TODO 3: change state and report mission KO
      case (AppState.Loading, Event.LoadMissionFailed(error)) =>
        ???

      // TODO 4: execute all commands and report mission result
      case (AppState.Ready(planet, rover), Event.CommandsReceived(commands)) =>
        ???

      // NOTE: otherwise something unknown happened so report mission KO
      case _ =>
        (AppState.Failed, Effect.ReportKo(new RuntimeException(s"Cannot handle $event event in $model state.")))
    }

  // NOTE: use the functions already seen to implement the effect logic
  def infrastructure(effect: Effect): IO[Option[Event]] =
    effect match {
      case Effect.LoadMission(planetFile, roverFile) =>
        def toFailed(t: Throwable): Event = Event.LoadMissionFailed(t)
        def toSuccessful(planet: Planet, rover: Rover): Event = Event.LoadMissionSuccessful(planet, rover)

        val loadResult =
          for {
            planet <- loadPlanet(planetFile)
            rover <- loadRover(roverFile)
          } yield (planet, rover)

        loadResult
          .map(toSuccessful)
          .recover(toFailed(_))
          // NOTE: signal to the Runtime to continue the loop
          .map(continue)

      case Effect.AskCommands =>
        loadCommands()
          .map(Event.CommandsReceived.apply)
          // NOTE: signal to the Runtime to continue the loop
          .map(continue)

      case Effect.ReportObstacleDetected(rover) =>
        writeObstacleDetected(rover)
          // NOTE: signal to the Runtime to stop the loop
          .map(stop)

      case Effect.ReportCommandSequenceCompleted(rover) =>
        writeSequenceCompleted(rover)
          // NOTE: signal to the Runtime to stop the loop
          .map(stop)

      case Effect.ReportKo(error) =>
        writeError(error)
          // NOTE: signal to the Runtime to stop the loop
          .map(stop)
    }

  // NOTE: we signal the desire to continue or stop with Option
  def continue(ev: Event): Option[Event] = Some(ev)
  def stop(ignore: Unit): Option[Event] = None

  // NOTE: wire together init, update and infrastructure
  def createApplication(planetFile: String, roverFile: String): IO[Unit] =
    Runtime.start(init(planetFile, roverFile), update, infrastructure)

  object Runtime {

    // NOTE: runtime loop implemented as recursive function
    def start[MODEL, EVENT, EFFECT](
      init: => (MODEL, EFFECT),
      update: (MODEL, EVENT) => (MODEL, EFFECT),
      infrastructure: EFFECT => IO[Option[EVENT]]
    ): IO[Unit] = {

      def loop(currentState: MODEL, currentEffect: EFFECT): IO[Unit] =
        infrastructure(currentEffect)
          .flatMap { wishToContinue =>
            wishToContinue match {
              case Some(ev) =>
                val (nextState, nextEffect) = update(currentState, ev)
                loop(nextState, nextEffect)
              case None => IO.unit
            }
          }

      val (beginModel, beginEffect) = init
      loop(beginModel, beginEffect)
    }
  }

  // INFRASTRUCTURE
  def eitherToIO[A](value: Either[ParseError, A]): IO[A] =
    IO.fromEither(value.leftMap(e => new RuntimeException(renderError(e))))

  def loadPlanet(file: String): IO[Planet] =
    loadTuple(file)
      .map(parsePlanet)
      .flatMap(eitherToIO)

  def loadRover(file: String): IO[Rover] =
    loadTuple(file)
      .map(parseRover)
      .flatMap(eitherToIO)

  def loadCommands(): IO[List[Command]] =
    ask("Waiting commands...")
      .map(parseCommands)

  def writeSequenceCompleted(rover: Rover): IO[Unit] =
    logInfo(renderComplete(rover))

  def writeObstacleDetected(rover: ObstacleDetected): IO[Unit] =
    logInfo(renderObstacle(rover))

  def writeError(error: Throwable): IO[Unit] =
    logError(error.getMessage)

  // PARSING
  def parseCommand(input: Char): Command =
    input.toString.toLowerCase match {
      case "f" => MoveForward
      case "b" => MoveBackward
      case "r" => TurnRight
      case "l" => TurnLeft
      case _   => Unknown
    }

  def parseCommands(input: String): List[Command] =
    input.map(parseCommand).toList

  def parseRover(input: (String, String)): Either[ParseError, Rover] = {
    val (inputPosition, inputOrientation) = input
    for {
      position <- parsePosition(inputPosition)
      orientation <- parseOrientation(inputOrientation)
    } yield Rover(position, orientation)
  }

  def parseOrientation(input: String): Either[ParseError, Orientation] =
    input.trim.toLowerCase match {
      case "n" => Right(N)
      case "w" => Right(W)
      case "e" => Right(E)
      case "s" => Right(S)
      case _   => Left(InvalidRover(s"invalid orientation: $input"))
    }

  def parsePosition(input: String): Either[ParseError, Position] =
    parseIntTuple(",", input)
      .map(Position.apply)
      .leftMap(_ => InvalidRover(s"invalid position: $input"))

  def parseSize(input: String): Either[ParseError, Size] =
    parseIntTuple("x", input)
      .map(Size.apply)
      .leftMap(_ => InvalidPlanet(s"invalid size: $input"))

  def parseObstacle(input: String): Either[ParseError, Obstacle] =
    parsePosition(input)
      .map(Obstacle.apply)
      .leftMap(_ => InvalidPlanet(s"invalid obstacle: $input"))

  def parseObstacles(input: String): Either[ParseError, List[Obstacle]] =
    input.split(" ").toList.traverse(parseObstacle)

  def parsePlanet(input: (String, String)): Either[ParseError, Planet] = {
    val (inputSize, inputObstacles) = input
    for {
      size <- parseSize(inputSize)
      obstacles <- parseObstacles(inputObstacles)
    } yield Planet(size, obstacles)
  }

  def parseIntTuple(separator: String, input: String): Either[Throwable, (Int, Int)] =
    Either.catchNonFatal {
      val parts = input.split(separator).toList
      (parts(0).trim.toInt, parts(1).trim.toInt)
    }

  // RENDERING
  def renderError(error: ParseError): String =
    error match {
      case InvalidPlanet(message) => s"Planet parsing: $message"
      case InvalidRover(message)  => s"Rover parsing: $message"
    }

  def renderComplete(rover: Rover): String =
    s"${rover.position.x}:${rover.position.y}:${rover.orientation}"

  def renderObstacle(hit: ObstacleDetected): String =
    s"O:${renderComplete(hit.rover)}"

  // DOMAIN
  def executeAll(planet: Planet, rover: Rover, commands: List[Command]): Either[ObstacleDetected, Rover] =
    commands.foldLeft(rover.asRight)((prev, cmd) => prev.flatMap(execute(planet, _, cmd)))

  def execute(planet: Planet, rover: Rover, command: Command): Either[ObstacleDetected, Rover] =
    command match {
      case TurnRight    => turnRight(rover).asRight
      case TurnLeft     => turnLeft(rover).asRight
      case MoveForward  => moveForward(planet, rover)
      case MoveBackward => moveBackward(planet, rover)
      case Unknown      => rover.asRight
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

  def moveForward(planet: Planet, rover: Rover): Either[ObstacleDetected, Rover] =
    next(planet, rover, delta(rover.orientation))
      .map(x => rover.copy(position = x))

  def moveBackward(planet: Planet, rover: Rover): Either[ObstacleDetected, Rover] =
    next(planet, rover, delta(opposite(rover.orientation)))
      .map(x => rover.copy(position = x))

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

  def next(planet: Planet, rover: Rover, delta: Delta): Either[ObstacleDetected, Position] = {
    val position = rover.position
    val candidate = position.copy(
      x = wrap(position.x, planet.size.width, delta.x),
      y = wrap(position.y, planet.size.height, delta.y)
    )
    val hitObstacle = planet.obstacles.map(_.position).contains(candidate)
    Either.cond(!hitObstacle, candidate, ObstacleDetected(rover))
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
  case class ObstacleDetected(rover: Rover)

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
