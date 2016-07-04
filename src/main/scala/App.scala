import cats.Id
import cats.arrow.NaturalTransformation
import cats.data.Coproduct
import cats.free.{ Coyoneda, Free }
import cats.implicits._

import scala.language.higherKinds

import Coproduct.{ leftc, rightc }

case class Button(label: String)

sealed trait ControlPanel[T]
case object ButtonPressed extends ControlPanel[Button]
case object StopEnabled extends ControlPanel[Unit]
case object StopDisabled extends ControlPanel[Unit]
case object CurrentFloor extends ControlPanel[Int]

sealed trait ElevatorControl[T]
case object GetFloor extends ElevatorControl[Int]
case class QueueFloor(x: Int) extends ElevatorControl[Unit]

trait FreeCSupport {
  type FreeC[F[_], A] = Free[({ type CoF[T] = Coyoneda[F,T] })#CoF,A]
  def liftFC[F[_], A](value: F[A]): FreeC[F, A] = Free.liftF[({ type CoF[α] = Coyoneda[F, α] })#CoF, A](Coyoneda.lift[F, A](value))
  def runFC[F[_], A](prog: FreeC[F, A])(implicit interp: NaturalTransformation[F, Id]): A = {
    prog.go { sfsa: Coyoneda[F, FreeC[F,A]] =>
      sfsa.transform(interp).run
    }
  }

  implicit def combineNT[F[_], G[_], H[_]](implicit f: NaturalTransformation[F, H], g: NaturalTransformation[G, H]) = new NaturalTransformation[({ type FG[A] = Coproduct[F, G, A] })#FG, H] {
    def apply[A](fga: Coproduct[F, G, A]): H[A] = {
      fga.fold(f, g)
    }
  }
}

trait FreeCTest1Interp {
  implicit val controlPanelInterp = new NaturalTransformation[ControlPanel, Id] {
    def apply[A](fa: ControlPanel[A]): Id[A] = fa match {
      case ButtonPressed => Button("Floor 2")
      case StopEnabled => println("Stop enabled")
      case StopDisabled => println("Stop disabled")
      case CurrentFloor => 2
    }
  }

  implicit val elevatorControlInterp = new NaturalTransformation[ElevatorControl, Id] {
    def apply[A](fa: ElevatorControl[A]): Id[A] = fa match {
      case GetFloor => 2
      case QueueFloor(floor) => ()
    }
  }
}

object FreeCTest1 extends App with FreeCSupport with FreeCTest1Interp {
  // Works, using FreeC only, no Coproduct
  val prog1: FreeC[ControlPanel, Int] = for {
    button <- liftFC(ButtonPressed)
    _ <- liftFC(StopEnabled)
    _ <- liftFC(StopDisabled)
    currentFloor <- liftFC(CurrentFloor)
  } yield { println(button); currentFloor }

  // Works when specifying all types everywhere.
  def prog(interp: NaturalTransformation[({ type CE[A] = Coproduct[ControlPanel, ElevatorControl, A] })#CE, Id]): Int = {
    val _prog = for {
      button <- liftFC[({ type CE[A] = Coproduct[ControlPanel, ElevatorControl, A] })#CE, Button](leftc(ButtonPressed))
      currentFloor2 <- liftFC[({ type CE[A] = Coproduct[ControlPanel, ElevatorControl, A] })#CE, Int](rightc(GetFloor))
      currentFloor <- liftFC[({ type CE[A] = Coproduct[ControlPanel, ElevatorControl, A] })#CE, Int](leftc(CurrentFloor))
    } yield { println(button); currentFloor2 }

    runFC[({ type CE[A] = Coproduct[ControlPanel, ElevatorControl, A] })#CE, Int](_prog)(interp)
  }
  println(prog(combineNT[ControlPanel, ElevatorControl, Id]))

  // Simple example of the above
  val coValue = Coyoneda.lift[({ type FG[A] = Coproduct[ControlPanel, ElevatorControl, A] })#FG, Button](leftc(ButtonPressed))
  val res = coValue.transform(combineNT).run
  println(res)
}
