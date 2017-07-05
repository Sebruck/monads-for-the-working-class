package monads

import monads.YourFunctors.Functor
import monads.YourMonads.Monad
import org.scalatest.{AsyncWordSpecLike, Matchers}

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.ExecutionContext.global
import scala.util.{Failure, Success, Try}

class MonadSpec extends AsyncWordSpecLike with Matchers {

  // slides at https://speakerdeck.com/yannickcw/monads-for-the-working-class
  import YourMonads.MonadOps

  "You" should {
    // easy, look at the slides if you need guidance :)
    "create a Monad for Option" in {
      import YourMonads.optionMonad

      def stringToInt(s: String): Option[Int] =
        Try(s.toInt).toOption

      Option("42").yourFlatMap(stringToInt) shouldBe Some(42)

      Option("WTF?").yourFlatMap(stringToInt) shouldBe None

      val noneOption: Option[String] = None

      noneOption.yourFlatMap(stringToInt) shouldBe None
    }

    // easy
    "create a Monad for Lists - wow how creative" in {
      import YourMonads.listMonad

      def duplicate[A](a: A): List[A] = List(a, a)

      List(1, 2, 3, 4, 5).yourFlatMap(duplicate) shouldBe
        List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)

      List.empty[String].yourFlatMap(duplicate) shouldBe List.empty[String]
    }

    // medium
    "create a Monad for Future - now it gets difficult" in {
      import YourMonads.futureMonad

      def getName(userId: Int): Future[String] = Future.successful("Bob")
      def getAge(name: String): Future[Int]    = Future.successful(22)

      getName(2)
        .yourFlatMap(name =>
          getAge(name).map { age =>
            name shouldBe "Bob"
            age shouldBe 22
        })
    }

    // advanced
    "create a FutureOption Transformer - wow two monads create a new monad" in {
      import YourMonads.optionMonad
      import YourMonads.futureMonad
      import Transformer.FutureOption

      // mhh how to flatMap over a nested Monad?
      def thisCouldBeARealDb(id: Int): FutureOption[String] =
        FutureOption(Future.successful(Some("User")))

      def thisAsWell(name: String): FutureOption[Int] =
        FutureOption(Future.successful(Some(22)))

      val userAge: FutureOption[(String, Int)] = for {
        user <- thisCouldBeARealDb(2)
        age  <- thisAsWell(user)
      } yield (user, age)

      userAge.value.map(userAge => userAge shouldBe Some("User", 22))
    }
  }
}

object YourMonads {

  trait Monad[M[_]] extends Functor[M] {
    def pure[A](fa: A): M[A]

    def flatMap[A, B](fa: M[A], f: A => M[B]): M[B]

    // todo: make you life easier, implement map here once and for all!
    // tip: you can do it by just using pure and flatMap
     def map[A, B](fa: M[A])(f: (A) => B): M[B] = flatMap(fa, (a: A) => pure(f(a)))

    // todo: implement flatten in terms of flatMap or implement flatMap interms of flatten and map
     def flatten[A](fa: M[M[A]]): M[A] = flatMap[M[A], A](fa, identity)
  }

  // helper for using M(A).yourFlatMap..
  implicit class MonadOps[M[_], A](a: M[A])(implicit ev: Monad[M]) {
    def yourFlatMap[B](f: A => M[B]): M[B] = ev.flatMap(a, f)
  }

  // todo: challenge yourself - please do not use the `flatMap` or `map` from standard library
  implicit def optionMonad: Monad[Option] =  new Monad[Option] {
    override def pure[A](fa: A): Option[A] = Option(fa)

    override def flatMap[A, B](fa: Option[A], f: (A) => Option[B]): Option[B] =
      fa match {
        case Some(a) => f(a)
        case None => None
      }
  }

  implicit def listMonad: Monad[List] = new Monad[List] {
    override def pure[A](fa: A): List[A] = List(fa)

    override def flatMap[A, B](fa: List[A], f: (A) => List[B]): List[B] = {
      @tailrec
      def loop(current: List[A], acc: List[B]): List[B] = {
        current match {
          case Nil => acc
          case head :: rest => loop(rest, acc ++ f(head))
        }
      }

      loop(fa, Nil)
    }
  }

  // you might need Promise
  implicit def futureMonad(implicit ec: ExecutionContext): Monad[Future] = new Monad[Future] {

    override def pure[A](fa: A): Future[A] = Future.successful(fa)

    override def flatMap[A, B](fa: Future[A], f: (A) => Future[B]): Future[B] = {
      val p = Promise[B]()

      fa.onComplete {
        case Success(s) => p.completeWith(f(s))
        case Failure(fail) => p.failure(fail)
      }

      p.future
    }
  }

}
object Transformer {

  case class FutureOption[A](
      value: Future[Option[A]])(implicit ec: ExecutionContext, optMon: Monad[Option], futMonad: Monad[Future]) {

    def flatMap[B](f: A => FutureOption[B]): FutureOption[B] = FutureOption {
      futMonad.flatMap(value, { opt: Option[A] =>
        optMon
          .map(opt)(f)
          .getOrElse(FutureOption(futMonad.pure(None)))
          .value
      })
    }

    def map[B](f: A => B): FutureOption[B] =
      flatMap((a: A) => FutureOption.pure(f(a)))
  }

  object FutureOption {
    def pure[A](fa: A)
               (implicit ec: ExecutionContext, optMon: Monad[Option], futMonad: Monad[Future]): FutureOption[A] =
      FutureOption(futMonad.pure(optMon.pure(fa)))
  }
}
