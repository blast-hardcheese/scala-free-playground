import cats.Id
import cats.arrow.NaturalTransformation
import cats.data.Coproduct
import cats.data.Xor
import cats.free.{ Coyoneda, Free }
import cats.implicits._

import scala.language.{ higherKinds, implicitConversions }

import Coproduct.{ leftc, rightc }

trait FreeCSupport {
  type FreeC[F[_], A] = Free[({ type CoF[T] = Coyoneda[F,T] })#CoF,A]
  def liftFC[F[_], A](value: F[A]): FreeC[F, A] = Free.liftF[({ type CoF[α] = Coyoneda[F, α] })#CoF, A](Coyoneda.lift[F, A](value))
  def runFC[F[_], A](prog: FreeC[F, A])(implicit interp: NaturalTransformation[F, Id]): A = {
    prog.go { sfsa: Coyoneda[F, FreeC[F,A]] =>
      sfsa.transform(interp).run
    }
  }

  def combineNT[F[_], G[_], H[_]](implicit f: NaturalTransformation[F, H], g: NaturalTransformation[G, H]) = new NaturalTransformation[({ type FG[A] = Coproduct[F, G, A] })#FG, H] {
    def apply[A](fga: Coproduct[F, G, A]): H[A] = {
      fga.fold(f, g)
    }
  }

  implicit def liftXorL[F[_], G[_], T](x: F[T]): F[T] Xor G[T] = Xor.left(x)
  implicit def liftXorR[F[_], G[_], T](x: G[T]): F[T] Xor G[T] = Xor.right(x)

  implicit def liftCoL[F[_], G[_], T](x: F[T]): Coproduct[F, G, T] = Coproduct(x)
  implicit def liftCoR[F[_], G[_], T](x: G[T]): Coproduct[F, G, T] = Coproduct(x)
}

case class Button(label: String)

sealed trait ControlPanel[T]
case object ButtonPressed extends ControlPanel[Button]
case object StopEnabled extends ControlPanel[Unit]
case object StopDisabled extends ControlPanel[Unit]
case object CurrentFloor extends ControlPanel[Int]

sealed trait CallButton[T]
case class CallElevator(floor: Int) extends CallButton[Unit]

sealed trait MotorControl[T]
case object GetSpeed extends MotorControl[Int]
case class SetSpeed(x: Int) extends MotorControl[Unit]

sealed trait ElevatorControl[T]
case object GetFloor extends ElevatorControl[Int]
case class QueueFloor(x: Int) extends ElevatorControl[Unit]

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

  implicit val callButtonInterp = new NaturalTransformation[CallButton, Id] {
    def apply[A](fa: CallButton[A]): Id[A] = fa match {
      case CallElevator(floor) => ()
    }
  }

  implicit val motorControlInterp = new NaturalTransformation[MotorControl, Id] {
    def apply[A](fa: MotorControl[A]): Id[A] = fa match {
      case GetSpeed => 5
      case SetSpeed(speed) => ()
    }
  }
}

object FreeCTest1 extends App with FreeCSupport with FreeCTest1Interp {
  type ProgramCPEC[T] = Coproduct[ControlPanel, ElevatorControl, T]
  type ProgramCBP1[T] = Coproduct[CallButton, ProgramCPEC, T]
  type ProgramMCP2[T] = Coproduct[MotorControl, ProgramCBP1, T]
  type Program[T]     = ProgramMCP2[T]

  implicit def liftCB[T](value: CallButton[T]) =      liftFC[Program, T](liftCoR(value))
  implicit def liftCP[T](value: ControlPanel[T]) =    liftFC[Program, T](liftCoR(liftCoR(value)))
  implicit def liftEC[T](value: ElevatorControl[T]) = liftFC[Program, T](liftCoR(liftCoR(value)))
  implicit def liftMC[T](value: MotorControl[T]) =    liftFC[Program, T](value)

  implicit def lift[F[_], T](x: F[T])(implicit lifter: F[T] => FreeC[Program, T]) = lifter(x)

  def prog(interp: NaturalTransformation[Program, Id]): Int = {

    val _prog = for {
      button <- ButtonPressed
      currentFloor2 <- GetFloor
      speed <- GetSpeed
      currentFloor <- CurrentFloor
      _ <- CallElevator(currentFloor2 + 1)
    } yield { println(button); currentFloor2 }

    runFC(_prog)(interp)
  }

  // Just figure it out, please.
  println(prog(combineNT(implicitly, combineNT(implicitly, combineNT))))
}
