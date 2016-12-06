//package pub.jayden.scala.fis
//
//import pub.jayden.scala.fis.chapter8.{Gen, Prop, SGen}
//
//import scala.util.matching.Regex
//
//package object chapter9 {
//
//  trait Parser{}
//  trait ParserError{}
//
//  trait Parsers[Parser[+_]]{ self =>
//
//    /*
//    * input을 받아서 파서를 실행해서 결과 리턴
//    *
//    * run(char(c))(c.toString) == Right(c)
//    * */
//    def run[A](p: Parser[A])(input: String): Either[ParserError, A]
//    /*
//    * s1 또는 s2 파서
//    * */
//    def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
//
//    /*
//    * p 파서가 n번
//    * */
//    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
//
//    def slice[A](p: Parser[A]): Parser[String]
//
//    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
//
//    /*
//    * 하나의 문자를 인식하는 파서
//    * */
//    def char(c: Char): Parser[Char] =
//      string(c.toString) map (_.charAt(0))
//
//    def succeed[A](a: A): Parser[A] =
//      string("") map (_ => a)
//
//    def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
//      for{ a <- p; b <- p2} yield (a, b)
//    //      flatMap(p)( a => map(p2)( b => (a, b) ))
//
//
//    //    def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
//    //      map(product(p, p2))( a => f(a._1, a._2) )
//
//    def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
//      for{ a <- p; b <- p2 } yield f(a, b)
//    //      flatMap(p)( a => map(p2)( b => f(a, b)))
//
//    def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
//      flatMap(p)( a => succeed(f(a)))
//
//    def map2_1[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C]
//    def product_1[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
//      map2_1(p, p2)( (a, b) => (a, b))
//
//    def many1[A](p: Parser[A]): Parser[List[A]] =
//      map2(p, many(p))((a, b) => a :: b)
//
//    //or, map2, succeed
//    def many[A](p: Parser[A]): Parser[List[A]] =
//      map2(p, many(p))(_ :: _) or succeed(List())
//
//
//    //7장 delay 참조..
//    def delay[A](p: => Parser[A]): Parser[A] = p
//
//    def many[A](p: Parser[A]): Parser[List[A]] =
//      map2(p, delay(many(p)))(_ :: _) or succeed(List())
//
//
//    def test9_6() = "[0-9]+".r.flatMap(a => listOfN(a.toInt, char('a')))
//
//
//    def label[A](msg: String)(p: Parser[A]): Parser[A]
//
//    case class Location(input: String, offset: Int = 0) {
//      lazy val line = input.slice(0, offset+1).count(_ == '\n') + 1
//      lazy val col = input.slice(0, offset+1).lastIndexOf('\n') match {
//        case -1 => offset + 1
//        case lineStart => offset - lineStart
//      }
//    }
//
//    def errorLocation(e: ParserError): Location
//    def errorMessage(e: ParserError): String
//
//
//    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
//      Prop.forAll(inputs ** Gen.string) {
//        case (input, msg) =>
//          run(label(msg)(p))(input) match {
//            case Left(e) => errorMessage(e) == msg
//            case _ => true
//          }
//      }
//
//    def scope[A](msg: String)(p: Parser[A]): Parser[A]
//
//
//    def removeLeft[A](p: Parser[A], remove: Parser[Any]): Parser[A] =
//      remove.slice.map2(p)((_,b) => b)
//
//    def removeRight[A](p: Parser[A], remove: Parser[Any]): Parser[A] =
//      p.map2(remove.slice)((a,_) => a)
//
//    def between[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) =
//      removeRight(removeLeft(p, start), stop)
//
//    /*
//    * aaa,bbb,ccc,ddd
//    * to
//    * List("aaa", "bbb", "ccc", "ddd")
//    * */
//    def split[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
//      map2(p, many(removeLeft(p, p2)))(_ :: _) or succeed(List())
//
//    /*
//    * 문자열을 인식하는 파서
//    * run(string(s))(s) == Right(s)
//    * */
//    implicit def string(s: String): Parser[String]
//    implicit def regex(r: Regex): Parser[String]
//
//    implicit def operators[A](p: Parser[A]):ParserOps[A] = ParserOps[A](p)
//    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
//
//    case class ParserOps[A](p: Parser[A]) {
//      def |[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
//      def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
//
//      def many: Parser[List[A]] = self.many(p)
//      def map[B](f: A => B): Parser[B] = self.map(p)(f)
//      def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)
//
//      def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
//      def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
//
//      def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
//      def slice: Parser[String] = self.slice(p)
//
//
//    }
//
//    case class ParserError(stack: List[(Location, String)])
//
//    object Laws{
//      def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
//        Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))
//
//      def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
//        equal(p, p.map(a => a))(in)
//    }
//
//  }
//
//  var samplJson =
//    """
//      {
//        "Company name" : "Microsoft Corporation",
//        "Ticker"  : "MSFT",
//        "Active"  : true,
//        "Price"   : 30.66,
//        "Shares outstanding" : 8.38e9,
//        "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
//      }
//    """.stripMargin
//
//  trait JSON
//  object JSON {
//    case object JNull extends JSON
//    case class JNumber(get: Double) extends JSON
//    case class JString(get: String) extends JSON
//    case class JBool(get: Boolean) extends JSON
//    case class JArray(get: IndexedSeq[JSON]) extends JSON
//    case class JObject(get: Map[String, JSON]) extends JSON
//
//    /*
//    *
//    * 문자열 리터럴을 위한 조합기
//    * number / string / bool / array / object
//    *
//    *
//    * */
//
//    def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
//      import P._
//
//
//
//      //      def array = between("[", "]")()
//
//      succeed(JNull)
//    }
//  }
//
//
//
//}
