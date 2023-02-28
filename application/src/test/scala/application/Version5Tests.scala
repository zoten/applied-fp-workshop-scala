package application

import munit.CatsEffectSuite

// TODO: Remove IgnoreSuite annotation

// @munit.IgnoreSuite
class Version5Tests extends CatsEffectSuite {

  import application.Version5._
  import application.Version5.Orientation._, Command._, ParseError._
  import cats.effect.{ IO, Ref }
  import scala.Console.{ GREEN, RED, RESET }

  test("go to opposite angle") {
    val result = for {
      output <- IO.ref("")

      // Initialize fake adapters
      stubPlanetReader = createPlanetReader(Planet(Size(5, 4), List(Obstacle(Position(2, 0)), Obstacle(Position(0, 3)))))
      stubRoverReader = createRoverReader(Rover(Position(0, 0), N))
      stubCommandsReader = createCommandsReader(
        List(TurnRight, MoveBackward, MoveBackward, TurnLeft, MoveBackward, TurnRight, MoveForward)
      )
      spyDisplayWriter = createDisplayWriter(output)

      // Wiring dependencies
      app <- createApplication(stubPlanetReader, stubRoverReader, stubCommandsReader, spyDisplayWriter)

      value <- output.get
    } yield value

    assertIO(result, s"INFO - 4:3:E")
  }

  test("error on loading planet data") {
    val result = for {
      output <- IO.ref("")

      // Initialize fake adapters
      stubPlanetReader = createPlanetReader(Left(InvalidPlanet("some error")))
      stubRoverReader = createRoverReader(Rover(Position(0, 0), N))
      stubCommandsReader = createCommandsReader(List(TurnRight, MoveForward, MoveForward))
      spyDisplayWriter = createDisplayWriter(output)

      // Wiring dependencies
      app <- createApplication(stubPlanetReader, stubRoverReader, stubCommandsReader, spyDisplayWriter)

      value <- output.get
    } yield value

    assertIO(result, s"ERROR - Planet parsing: some error")
  }

  test("hit obstacle during commands execution (integration test)") {
    val app = createApplication("planet.txt", "rover.txt")
    val result = runCaptureOutput("RFF", app)
    assertIO(result, s"$GREEN[OK] O:1:0:E$RESET")
  }

  def createPlanetReader(planet: Planet): PlanetReader =
    () => IO(planet)

  def createPlanetReader(error: Left[ParseError, Planet]): PlanetReader =
    () => eitherToIO(error)

  def createRoverReader(rover: Rover): RoverReader =
    () => IO(rover)

  def createCommandsReader(commands: List[Command]): CommandsReader =
    () => IO(commands)

  def createDisplayWriter(output: Ref[IO, String]): DisplayWriter =
    new DisplayWriter {
      def sequenceCompleted(rover: Rover): IO[Unit] = output.set(s"INFO - ${renderComplete(rover)}")
      def obstacleDetected(rover: ObstacleDetected): IO[Unit] = output.set(s"INFO - ${renderObstacle(rover)}")
      def error(error: Throwable): IO[Unit] = output.set(s"ERROR - ${error.getMessage}")
    }

  def runCaptureOutput[A](commands: String, program: IO[Unit]): IO[String] = IO {
    import java.io.{ ByteArrayInputStream, ByteArrayOutputStream, PrintStream }

    val originalIn = System.in
    val originalOut = System.out
    try {
      val in = new ByteArrayInputStream(s"$commands\n".getBytes())
      val out = new ByteArrayOutputStream

      System.setIn(in)
      System.setOut(new PrintStream(out))
      program.unsafeRunSync()
      out.toString.replace("\r", "").split('\n').last
    } finally {
      System.setIn(originalIn)
      System.setOut(originalOut)
    }
  }
}
