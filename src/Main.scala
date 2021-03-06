object Main {

  sealed abstract class Language {
    override def toString: String = s"The language is: ${
      this match {
        case Finite => "finite"
        case Regular => "regular"
        case DCFL => "deterministic context-free"
        case CFL => "context-free"
        case Recursive => "recursive"
        case RE => "recursively enumerable"
        case Unknown => "unknown"
        case _ => this
      }
    }"
  }

  case object Finite extends Language
  case object Regular extends Language
  case object DCFL extends Language
  case object CFL extends Language
  case object Recursive extends Language
  case object RE extends Language
  case object Unknown extends Language
  case class BinOp(x: Language, op: BinOperation, y: Language) extends Language
  case class UnOp(x: Language, op: UnOperation) extends Language

  sealed abstract class Operation
  sealed abstract class BinOperation extends Operation
  case object Union extends BinOperation
  case object Intersection extends BinOperation
  case object Concat extends BinOperation

  sealed abstract class UnOperation extends Operation
  case object Star extends UnOperation
  case object Complement extends UnOperation

  def main(args: Array[String]): Unit = {

    two(CFL, Intersection, UnOp(Regular, Complement))


    // two(Regular, Intersection, RE)
    // three(Finite, Intersection, CFL, Intersection, UnOp(Finite, Complement))
    // four(Regular, Intersection, Regular, Intersection, CFL, Intersection, Regular)
  }

  def two(firstL: Language, firstOp: BinOperation, secondL: Language): Unit = {
    testClosure(BinOp(firstL, firstOp, secondL))
  }

  def three(firstL: Language, firstOp: BinOperation, secondL: Language, secondOp: BinOperation, thirdL: Language): Unit = {
    testClosure(BinOp(thirdL, secondOp, BinOp(firstL, firstOp, secondL)))
  }

  def four(firstL: Language, firstOp: BinOperation, secondL: Language, secondOp: BinOperation, thirdL: Language, thirdOp: BinOperation, fourthL: Language): Unit = {
    testClosure(BinOp(fourthL, thirdOp, BinOp(thirdL, secondOp, BinOp(firstL, firstOp, secondL))))
  }

  def testClosure(prg: Language): Unit = {
    println(closure(prg))
  }

  def closure(x: Language): Language = x match {
    case Finite => Finite
    case Regular => Regular
    case DCFL => DCFL
    case CFL => CFL
    case RE => RE
    case Recursive => Recursive
    case Unknown => Unknown
    case UnOp(x, op) =>
      val languageval = closure(x)
      op match {
      case Star => languageval match {
        case Finite => Unknown
        case Regular => Regular
        case DCFL => Unknown
        case CFL => CFL
        case Recursive => Recursive
        case RE => RE
        case Unknown => Unknown
        case _ => throw new Exception
      }
      case Complement => languageval match {
        case Finite => Unknown
        case Regular => Regular
        case DCFL => DCFL
        case CFL => Unknown
        case Recursive => Recursive
        case RE => Unknown
        case Unknown => Unknown
        case _ => throw new Exception
      }
      case _ => throw new Exception
    }
    case BinOp(x, op, y) =>
      val xClosure = closure(x)
      val yClosure = closure(y)
      op match {
        case Union => (xClosure, yClosure) match {
          // regular
          case (Regular, Regular | Finite) => Regular
          case (Regular | Finite, Regular) => Regular
          // cfl
          case (CFL, Regular | Finite | CFL) => CFL
          case (CFL | Finite | Regular, CFL) => CFL
          // recursive
          case (Recursive, Regular | Finite | CFL | DCFL | Recursive) => Recursive
          case (Recursive | Regular | Finite | CFL | DCFL, Recursive) => Recursive
          // re
          case (RE, Regular | Finite | CFL | DCFL | Recursive | RE) => RE
          case (RE | Regular | Finite | CFL | DCFL | Recursive, RE) => RE
          case (_, _) => Unknown
        }
        case Intersection => (xClosure, yClosure) match {
          case (Finite, _) => Finite
          case (_, Finite) => Finite
          // regular
          case (Regular, Regular) => Regular
          // cfl
          case (CFL, Regular) => CFL
          case (Regular, CFL) => CFL
          // recursive
          case (Recursive, Regular | CFL | DCFL | Recursive) => Recursive
          case (Recursive | Regular | CFL | DCFL, Recursive) => Recursive
          // re
          case (RE, Regular | CFL | DCFL | Recursive | RE) => RE
          case (RE | Regular | CFL | DCFL | Recursive, RE) => RE
          case (_, _) => Unknown
        }
        case Concat =>(xClosure, yClosure) match {
          // finite
          case (Finite, Finite) => Finite
          // regular
          case (Regular, Regular | Finite) => Regular
          case (Regular | Finite, Regular) => Regular
          // cfl
          case (CFL, Regular | Finite | CFL) => CFL
          case (CFL | Finite | Regular, CFL) => CFL
          // recursive
          case (Recursive, Regular | Finite | CFL | DCFL | Recursive) => Recursive
          case (Recursive | Regular | Finite | CFL | DCFL, Recursive) => Recursive
          // re
          case (RE, Regular | Finite | CFL | DCFL | Recursive | RE) => RE
          case (RE | Regular | Finite | CFL | DCFL | Recursive, RE) => RE
          case (_, _) => Unknown
        }
        case _ => throw new Exception
      }
    case _ => throw new Exception
  }
}
