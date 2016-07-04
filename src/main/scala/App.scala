import cats.Id
import cats.arrow.NaturalTransformation
import cats.data.Coproduct
import cats.data.Xor
import cats.free.{ Coyoneda, Free }
import cats.implicits._

import scala.language.{ higherKinds, implicitConversions }

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

  implicit def liftXorL[F[_], G[_], T](x: F[T]): F[T] Xor G[T] = Xor.left(x)
  implicit def liftXorR[F[_], G[_], T](x: G[T]): F[T] Xor G[T] = Xor.right(x)

  implicit def liftCoL[F[_], G[_], T](x: F[T]): Coproduct[F, G, T] = Coproduct(x)
  implicit def liftCoR[F[_], G[_], T](x: G[T]): Coproduct[F, G, T] = Coproduct(x)
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
    implicit def liftCP[T](value: ControlPanel[T]) = liftFC[({ type CE[A] = Coproduct[ControlPanel, ElevatorControl, A] })#CE, T](value)
    implicit def liftEC[T](value: ElevatorControl[T]) = liftFC[({ type CE[A] = Coproduct[ControlPanel, ElevatorControl, A] })#CE, T](value)
    def lift[F[_], T](x: F[T])(implicit lifter: F[T] => FreeC[({ type CE[A] = Coproduct[ControlPanel, ElevatorControl, A] })#CE, T]) = lifter(x)

    val _prog = for {
      button <- lift(ButtonPressed)
      currentFloor2 <- lift(GetFloor)
      currentFloor <- lift(CurrentFloor)
    } yield { println(button); currentFloor2 }

    runFC[({ type CE[A] = Coproduct[ControlPanel, ElevatorControl, A] })#CE, Int](_prog)(interp)
  }
  println(prog(combineNT[ControlPanel, ElevatorControl, Id]))

  // Simple example of the above
  val coValue = Coyoneda.lift[({ type FG[A] = Coproduct[ControlPanel, ElevatorControl, A] })#FG, Button](leftc(ButtonPressed))
  val res = coValue.transform(combineNT).run
  println(res)
}
