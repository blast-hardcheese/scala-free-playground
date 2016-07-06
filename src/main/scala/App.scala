import cats.{ Functor, Id, Monad }
import cats.arrow.Arrow
import cats.arrow.NaturalTransformation
import cats.data.{ Coproduct, Kleisli, WriterT, Xor, XorT }
import cats.free.{ Coyoneda, Free }
import cats.implicits._

import scala.language.{ higherKinds, implicitConversions }

import Coproduct.{ leftc, rightc }

case class SemanticArrow[A, B](f: A => B, label: Option[String]) {
  def apply(a: A): B = f(a)
}

object SemanticArrow {
  implicit val arrow = new Arrow[SemanticArrow] {
    def lift[A,B](f: A => B): SemanticArrow[A, B] = SemanticArrow(f, None)

    def first[A, B, C](fa: SemanticArrow[A,B]): SemanticArrow[(A, C),(B, C)] = {
      SemanticArrow[(A, C), (B, C)]({ case (a,c) => (fa.f(a), c) }, fa.label.map(label => s"$label, on the first"))
    }

    // Members declared in cats.arrow.Category
    def id[A]: SemanticArrow[A,A] = SemanticArrow(identity, None)

    def compose[A,B,C](f: SemanticArrow[B,C], g: SemanticArrow[A,B]): SemanticArrow[A,C] = {
      SemanticArrow(g.f andThen f.f, (for { lg <- g.label; lf <- f.label } yield Some(s"${lg} -> ${lf}")).getOrElse(g.label <+> f.label))
    }
  }

  implicit class LabelSyntax[A, B](sa: SemanticArrow[A, B]) {
    def ?(label: String): SemanticArrow[A, B] = sa.copy(label = Some(label))
  }
  def lift[A, B](f: A => B)(label: String): SemanticArrow[A, B] = Arrow[SemanticArrow].lift(f) ? label
}

object ArrowTest1 extends App {
  val fab = Arrow[SemanticArrow].lift[Double, Double](_ + 3) ? "Add three to a double"
  val f: (Int => Double) = _.toDouble / 2
  val g: (Double => String) = x => (x * 3).toString
  val dimapArrow = Arrow[SemanticArrow].dimap(fab)(f)(g)
  println(fab.label)
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
  def runFC[F[_], G[_], A](prog: FreeC[F, A], interp: NaturalTransformation[F, G])(implicit G: Monad[G]): G[A] = {
    prog.runM { sfsa: Coyoneda[F, FreeC[F,A]] =>
      val x: G[FreeC[F, A]] = sfsa.transform(interp).run
      x
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
  type LoggingId[A] = WriterT[Id, List[String], A]

  def log[T](value: T)(msg: String)(implicit F: Functor[Id]): LoggingId[T] = WriterT.putT[Id, List[String], T](value)(List(msg))
  implicit def noLog[T](value: T): LoggingId[T] = WriterT.putT[Id, List[String], T](value)(List.empty)

  implicit val loggingIdInterp = new NaturalTransformation[LoggingId, Id] {
    def apply[A](fa: LoggingId[A]): Id[A] = { println(s"Logged: ${fa.written}"); fa.value }
  }

  implicit val controlPanelInterp = new NaturalTransformation[ControlPanel, LoggingId] {
    def apply[A](fa: ControlPanel[A]): LoggingId[A] = fa match {
      case ButtonPressed => log(Button("Floor 2"))("Pressed floor 2")
      case StopEnabled => log(())("Stop enabled")
      case StopDisabled => log(())("Stop disabled")
      case CurrentFloor => log(2)("Got current floor")
    }
  }

  implicit val elevatorControlInterp = new NaturalTransformation[ElevatorControl, LoggingId] {
    def apply[A](fa: ElevatorControl[A]): LoggingId[A] = fa match {
      case GetFloor => 2
      case QueueFloor(floor) => ()
    }
  }

  implicit val callButtonInterp = new NaturalTransformation[CallButton, LoggingId] {
    def apply[A](fa: CallButton[A]): LoggingId[A] = fa match {
      case CallElevator(floor) => log(())(s"Elevator called to floor $floor")
    }
  }

  implicit val motorControlInterp = new NaturalTransformation[MotorControl, LoggingId] {
    def apply[A](fa: MotorControl[A]): LoggingId[A] = fa match {
      case GetSpeed => for {
          randomValue <- log(5)("Generating totally random number")
          _           <- log(())("Killing time")
        } yield randomValue
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

  def prog(interp: NaturalTransformation[Program, LoggingId]): Id[Int] = {

    val _prog = for {
      button        <- ButtonPressed
      currentFloor2 <- GetFloor
      speed         <- GetSpeed
      _             <- StopEnabled
      currentFloor  <- CurrentFloor
      _             <- CallElevator(currentFloor2 + 1)
    } yield currentFloor2

    loggingIdInterp(runFC(_prog, interp))
  }

  // Just figure it out, please.
  println(prog(combineNT(implicitly, combineNT(implicitly, combineNT))))
}
