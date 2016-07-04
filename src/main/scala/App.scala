import cats.Id
import cats.arrow.Arrow
import cats.arrow.NaturalTransformation
import cats.data.Coproduct
import cats.data.{ Kleisli, Xor, XorT }
import cats.free.{ Coyoneda, Free }
import cats.implicits._

import scala.language.{ higherKinds, implicitConversions }

import Coproduct.{ leftc, rightc }

object ArrowTest1 extends App {
  val fab: (Double => Double) = _ + 0
  val f: (Int => Double) = _.toDouble / 2
  val g: (Double => String) = x => (x * 3).toString
  val dimapArrow = Arrow[Function1].dimap(fab)(f)(g)
  println(dimapArrow(1))
}

object KleisliTest1 extends App {
  val getInput: Kleisli[Option, Unit,  Int] = Kleisli(Unit => Option(5))
  val prompt:   Kleisli[Option,  Int, Unit] = Kleisli(i => Option(println(s"Please enter $i:")))
  val res:      Kleisli[Option, Unit, Unit] = getInput andThen prompt
  res.run(())
}

trait FreeCSupport {
  type FreeC[F[_], A] = Free[Coyoneda[F,?], A]
  def liftFC[F[_], A](value: F[A]): FreeC[F, A] = Free.liftF[Coyoneda[F, ?], A](Coyoneda.lift[F, A](value))
  def runFC[F[_], A](prog: FreeC[F, A])(implicit interp: NaturalTransformation[F, Id]): A = {
    prog.go { sfsa: Coyoneda[F, FreeC[F,A]] =>
      sfsa.transform(interp).run
    }
  }

  def combineNT[F[_], G[_], H[_]](implicit f: NaturalTransformation[F, H], g: NaturalTransformation[G, H]) = new NaturalTransformation[Coproduct[F, G, ?], H] {
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
  type ProgramL1[T] = Coproduct[ControlPanel, ElevatorControl, T]
  implicit def liftCP[T](value: ControlPanel[T]) =    liftFC[Program, T](liftCoR(liftCoR(value)))
  implicit def liftEC[T](value: ElevatorControl[T]) = liftFC[Program, T](liftCoR(liftCoR(value)))

  type ProgramL2[T] = Coproduct[CallButton, ProgramL1, T]
  implicit def liftCB[T](value: CallButton[T]) =      liftFC[Program, T](liftCoR(value))

  type ProgramMCP2[T] = Coproduct[MotorControl, ProgramL2, T]
  implicit def liftMC[T](value: MotorControl[T]) =    liftFC[Program, T](value)

  type Program[T]     = ProgramMCP2[T]

  implicit def lift[F[_], T](x: F[T])(implicit lifter: F[T] => FreeC[Program, T]) = lifter(x)

  def prog(interp: NaturalTransformation[Program, Id]): Int = {

    val _prog = for {
      button        <- ButtonPressed
      currentFloor2 <- GetFloor
      speed         <- GetSpeed
      currentFloor  <- CurrentFloor
      _             <- CallElevator(currentFloor2 + 1)
    } yield currentFloor2

    runFC(_prog)(interp)
  }

  // Just figure it out, please.
  println(prog(combineNT(implicitly, combineNT(implicitly, combineNT))))
}
