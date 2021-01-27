import cats.effect.IO
import cats.implicits._
import cats.effect.IOApp
import cats.effect.ExitCode

// Ask for the user name and age
// if name is jorge error you cannot ask his name
// if age < 50 log message including name and age and return success
// else succeed with no value

object CallMeJorge extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    val ass = IO(println("Hello")).map(* => ExitCode.Success)
    ass
  }
}
