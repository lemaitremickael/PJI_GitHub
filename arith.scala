/* TP 3: typing of arithmetic expressions.          */
/* Univ. Lille 1 - M1 Info - CALP                       */
/* v 1.0 - 2012-02-10                                   */
/* Licence Creative Commons BY SA                       */
/* @author Pierre Boulet <Pierre.Boulet@univ-lille1.fr> */

abstract class Expression {
  /** The type of expressions. */
  abstract class Expr
  abstract class Type

  /** The exception to throw new new when an expression is not evaluable. */
  case class NotEvaluable(message: String) extends Exception(message)
  case class TypeError(message: String) extends Exception(message)
  
  def inferType(e : Expr): Type

  /** Evaluates an expression to a value or fails with a NotEvaluable exception. */
  def evaluate(e: Expr): Expr
}

/** Basic expressions */
object Basic extends Expression {
  case class Add(e1: Expr, e2: Expr) extends Expr
  case class Sub(e1: Expr, e2: Expr) extends Expr
  case class Val(v: Double) extends Expr
  case class Opposite(e: Expr) extends Expr
  
  case object NumberT extends Type

  override def inferType(e: Expr): Type = e match{
    case Add(e1, e2) => (inferType(e1), inferType(e2)) match {
      case (NumberT, NumberT) => NumberT
      case (NumberT, ee)      => throw new TypeError(ee+" is not a good type.")
      case (ee, _)            => throw new TypeError(ee+" is not a good type.")
    }
    case Sub(e1, e2) => (inferType(e1), inferType(e2)) match {
      case (NumberT, NumberT) => NumberT
      case (NumberT, ee)      => throw new TypeError(ee+" is not a good type.")
      case (ee, _)            => throw new TypeError(ee+" is not a good type.")
    }
    case Opposite(e) => inferType(e) match {
      case NumberT => NumberT
      case ee     => throw new TypeError(ee+" is not a good type.")
    }
    case Val(_) => NumberT
    case _ => throw new TypeError("the argument is incorrect.")

  }
  
  override def evaluate(e: Expr): Expr = e match {
    case Add(e1, e2) => (evaluate(e1), evaluate(e2)) match {
      case (Val(v1), Val(v2)) => Val(v1 + v2)
      case (Val(v1), ee)      => throw new NotEvaluable(ee+" is not a value.")
      case (ee, _)            => throw new NotEvaluable(ee+" is not a value.")
    }
    case Sub(e1, e2) => (evaluate(e1), evaluate(e2)) match {
      case (Val(v1), Val(v2)) => Val(v1 - v2)
      case (Val(v1), ee)      => throw new NotEvaluable(ee+" is not a value.")
      case (ee, _)            => throw new NotEvaluable(ee+" is not a value.")
    }
    case Opposite(e) => evaluate(e) match {
      case Val(v) => Val(-v)
      case ee     => throw new NotEvaluable(ee+" is not a value.")
    }
    case _ => e
  }
}

/** Expressions with all arithmetic operations. */
object Ar extends Expression {
  /** Arithmetic operators */
  abstract class Operator
  case object Plus extends Operator
  case object Minus extends Operator
  case object Mult extends Operator
  case object Div extends Operator
  
  case object NumberT extends Type

  /** Arithmetic expressions */
  case class BinOp(op: Operator, e1: Expr, e2: Expr) extends Expr
  case class UnOp(op: Operator, e: Expr) extends Expr
  case class Val(v: Double) extends Expr

  override def inferType(e: Expr): Type = e match{
    case BinOp(o, e1, e2) => (o, inferType(e1), inferType(e2)) match {
      case(Plus, NumberT, NumberT) => NumberT
      case(Minus, NumberT, NumberT) => NumberT
      case(Mult, NumberT, NumberT) => NumberT
      case(Div, NumberT, NumberT) => NumberT
      case(_,NumberT, ee) => throw new TypeError(ee+" is not a number")
      case(_, ee, _) => throw new TypeError(ee+" is not a good type")
    }
    case UnOp(o, e1) => (o, inferType(e1)) match {
      case(Plus, NumberT) => NumberT
      case(Minus, NumberT) => NumberT
      case(ee, NumberT) => throw new TypeError("Syntax error: illegal unary operator "+o)
      case(_, ee) => throw new TypeError(ee+" is not a good type")
    }
    case Val(_)=> NumberT
    case _ => throw new TypeError("the argument is incorrect.")
}
  override def evaluate(e: Expr): Expr = e match {
    case BinOp(o, e1, e2) => (evaluate(e1), evaluate(e2)) match {
      case (Val(v1), Val(v2)) => o match {
        case Plus  => Val(v1 + v2)
        case Minus => Val(v1 - v2)
        case Mult  => Val(v1 * v2)
        case Div   => Val(v1 / v2)
      }
      case (Val(v1), ee) => throw new NotEvaluable(ee+" is not a value.")
      case (ee, _)       => throw new NotEvaluable(ee+" is not a value.")
    }
    case UnOp(o, e1) => o match {
      case Plus => evaluate(e1)
      case Minus => evaluate(e1) match {
        case Val(v) => Val(-v)
        case ee     => throw new NotEvaluable(ee+" is not a value.")
      }
      case _ => throw new NotEvaluable("Syntax error: illegal unary operator "+o)
    }
    case Val(v) => Val(v)
  }
}

/** Expressions with all arithmetic operations and conditionals. */
object ArCo extends Expression {
  /** Booleans */
  abstract class Boolean
  case object True extends Boolean
  case object False extends Boolean

  /** Arithmetic operators */
  abstract class Operator
  case object Plus extends Operator
  case object Minus extends Operator
  case object Mult extends Operator
  case object Div extends Operator
  
  case object NumberT extends Type
  case object BooleanT extends Type

  /** Arithmetic expressions */
  case class BinOp(op: Operator, e1: Expr, e2: Expr) extends Expr
  case class UnOp(op: Operator, e: Expr) extends Expr
  case class Val(v: Double) extends Expr
  case class Bool(b: Boolean) extends Expr
  case class Ite(c: Expr, t: Expr, e: Expr) extends Expr
  case class Eq(e1: Expr, e2: Expr) extends Expr

  override def inferType(e: Expr): Type = e match {
    case BinOp(o, e1, e2) => (o, inferType(e1), inferType(e2)) match {
      case (Plus, NumberT, NumberT) => NumberT
      case (Minus, NumberT, NumberT) => NumberT
      case (Mult, NumberT, NumberT) => NumberT
      case (Div, NumberT, NumberT) => NumberT  
      case (_, NumberT, ee) => throw new TypeError(ee+" is not a number.")
      case (_, ee, _)       => throw new TypeError(ee+" is not a good type.")
    }
    case UnOp(o, e1) => (o, inferType(e1)) match {
      case(Plus, NumberT) => NumberT
      case(Minus, NumberT) => NumberT
      case(_,ee) => throw new TypeError(ee+" is not a number.")
      case _ => throw new TypeError("Syntax error: illegal unary operator "+o)
    }
    case Val(_)  => NumberT
    case Bool(_) => BooleanT
    case Ite(c, t, e) => (inferType(c), t, e) match {
      case(BooleanT, t, e) if(inferType(t) == inferType(e))=> inferType(t)
      case(BooleanT, t, e) => throw new TypeError(t+" and "+e+" have not the same type.")
      case _ => throw new TypeError("Syntax error : not a boolean")
    }
    case Eq(e1, e2) => (inferType(e1), inferType(e2)) match {
      case(v1,v2) if(v1 == v2) => BooleanT
      case(_,_) => throw new TypeError("They are different.")
    }
  }
  override def evaluate(e: Expr): Expr = e match {
    case BinOp(o, e1, e2) => (evaluate(e1), evaluate(e2)) match {
      case (Val(v1), Val(v2)) => o match {
        case Plus  => Val(v1 + v2)
        case Minus => Val(v1 - v2)
        case Mult  => Val(v1 * v2)
        case Div   => Val(v1 / v2)
      }
      case (Val(v1), ee) => throw new NotEvaluable(ee+" is not a value.")
      case (ee, _)       => throw new NotEvaluable(ee+" is not a value.")
    }
    case UnOp(o, e1) => o match {
      case Plus => evaluate(e1)
      case Minus => evaluate(e1) match {
        case Val(v) => Val(-v)
        case ee     => throw new NotEvaluable(ee+" is not a value.")
      }
      case _ => throw new NotEvaluable("Syntax error: illegal unary operator "+o)
    }
    case Val(v)  => Val(v)
    case Bool(b) => Bool(b)
    case Ite(c, t, e) => evaluate(c) match {
      case Bool(True)  => evaluate(t)
      case Bool(False) => evaluate(e)
      case ce          => throw new NotEvaluable(ce+" is not a boolean")
    }
    case Eq(e1, e2) => (evaluate(e1), evaluate(e2)) match {
      case (Bool(b1), Bool(b2)) => Bool(if (b1 == b2) True else False)
      case (Val(v1), Val(v2))   => Bool(if (v1 == v2) True else False)
      case (ee1, ee2)           => throw new NotEvaluable(ee1+" and "+ee2+" are not comparable")
    }
  }
}

/** Expressions with all arithmetic operations, conditionals and variables. */
object ArCoEn extends Expression {
  /** Booleans */
  abstract class Boolean
  case object True extends Boolean
  case object False extends Boolean

  /** Arithmetic operators */
  abstract class Operator
  case object Plus extends Operator
  case object Minus extends Operator
  case object Mult extends Operator
  case object Div extends Operator

  /** Arithmetic expressions */
  case class BinOp(op: Operator, e1: Expr, e2: Expr) extends Expr
  case class UnOp(op: Operator, e: Expr) extends Expr
  case class Val(v: Double) extends Expr
  case class Bool(b: Boolean) extends Expr
  case class Ite(c: Expr, t: Expr, e: Expr) extends Expr
  case class Eq(e1: Expr, e2: Expr) extends Expr
  case class Var(x: String) extends Expr
  case class Def(x: String, e: Expr, in: Expr) extends Expr
  
  case object NumberT extends Type
  case object booleanT extends Type
  case class VarT extends Type
  case class DefT extends Type

  def infT(e : Expr, env : Map[String, Expr]) : Type = e match{
    case BinOp(op, e1, e2) => (op, inferType(e1), inferType(e2)) match{
      case (Plus,NumberT,NumberT) => NumberT
      case (Minus,NumberT,NumberT) => NumberT
      case (Mult,NumberT,NumberT) => NumberT
      case (Div,NumberT,NumberT) => NumberT
      case (_,NumberT,ee) => throw new TypeError(ee+" is not a good type.")
      case (_,ee,_) => throw new TypeError(ee+" is not a good type.")
    }
    case UnOp(op, e1) => (op, inferType(e1)) match{
      case (Plus,NumberT)=> NumberT
      case (Minus,NumberT) => NumberT
      case (ee,NumberT) => throw new TypeError(ee+" is not an operator.")
      case (_,ee) => throw new TypeError(ee+" is is not a good type.")
    }
    case Val(_) => NumberT
    case Bool(_) => booleanT
    case Ite(c, t, e) => (inferType(c), t, e) match{
      case(booleanT, t, e) if(inferType(t) == inferType(e))=> inferType(t)
      case(booleanT, t, e) => throw new TypeError(t+" and "+e+" have not the same type.")
      case _ => throw new TypeError("Syntax error : not a boolean.")
    }
    case Eq(e1, e2) => (inferType(e1), inferType(e2)) match{
      case(v1,v2) if(v1 == v2) => booleanT
      case(_,_) => throw new TypeError("They are different.")
    }
    case Def(x,e,in) => infT(in,env + (x -> e))
    case Var(x) => infT(env(x),env)
    case _ => throw new TypeError("Unknown type.")
  }
  
  /** Evaluates expression e in environment env. */
  def eval(e: Expr, env: Map[String, Expr]): Expr = e match {
    case BinOp(o, e1, e2) => (eval(e1, env), eval(e2, env)) match {
      case (Val(v1), Val(v2)) => o match {
        case Plus  => Val(v1 + v2)
        case Minus => Val(v1 - v2)
        case Mult  => Val(v1 * v2)
        case Div   => Val(v1 / v2)
      }
      case (Val(v1), ee) => throw new NotEvaluable(ee+" is not a value.")
      case (ee, _)       => throw new NotEvaluable(ee+" is not a value.")
    }
    case UnOp(o, e1) => o match {
      case Plus => eval(e1, env)
      case Minus => eval(e1, env) match {
        case Val(v) => Val(-v)
        case ee     => throw new NotEvaluable(ee+" is not a value.")
      }
      case _ => throw new NotEvaluable("Syntax error: illegal unary operator "+o)
    }
    case Val(v)  => Val(v)
    case Bool(b) => Bool(b)
    case Ite(c, t, e) => eval(c, env) match {
      case Bool(True)  => eval(t, env)
      case Bool(False) => eval(e, env)
      case ce          => throw new NotEvaluable(ce+" is not a boolean")
    }
    case Eq(e1, e2) => (eval(e1, env), eval(e2, env)) match {
      case (Bool(b1), Bool(b2)) => Bool(if (b1 == b2) True else False)
      case (Val(v1), Val(v2))   => Bool(if (v1 == v2) True else False)
      case (ee1, ee2)           => throw new NotEvaluable(ee1+" and "+ee2+" are not comparable")
    }
    case Var(x) => try env(x) catch {
      case ex: NoSuchElementException => throw new NotEvaluable("Undefined name "+x)
    }
    case Def(x, e1, in) => eval(in, env + (x -> eval(e1, env)))
  }

  override def inferType(e: Expr): Type = infT(e, Map())
  
  /** Evaluates an expression to a value or fails with a NotEvaluable exception. */
  override def evaluate(e: Expr): Expr = eval(e, Map())

  /* Expressions for tests. */
  val e1 = BinOp(Plus, Val(2), Val(3))
  val e2 = BinOp(Mult, Val(2), e1)
  val e3 = UnOp(Minus, e2)
  val e4 = BinOp(Minus, e2, Var("x"))
  val e5 = Def("x", e1, e4)
  val e6 = Eq(e1, e5)
  val e7 = Ite(e6, e2, e3)

  /** Tests the eval method. Should return true. */
  def test =
    eval(e1, Map()) == Val(5) &&
      eval(e2, Map()) == Val(10) &&
      eval(e3, Map()) == Val(-10) &&
      eval(e4, Map()) == BinOp(Minus, Val(10), Var("x")) &&
      eval(e4, Map("x" -> Val(10))) == Val(0) &&
      eval(e5, Map("x" -> Val(10))) == Val(5) &&
      eval(e6, Map()) == Bool(True) &&
      eval(e7, Map()) == Val(10)
}


/*
/** Expressions with all arithmetic operations, conditionals, variables and functions. */
object ArCoEnFo extends Expression {
  /** Booleans */
  abstract class Boolean
  case object True extends Boolean
  case object False extends Boolean

  /** Arithmetic operators */
  abstract class Operator
  case object Plus extends Operator
  case object Minus extends Operator
  case object Mult extends Operator
  case object Div extends Operator

  /** Arithmetic expressions */
  case class BinOp(op: Operator, e1: Expr, e2: Expr) extends Expr
  case class UnOp(op: Operator, e: Expr) extends Expr
  case class Val(v: Double) extends Expr
  case class Bool(b: Boolean) extends Expr
  case class Ite(c: Expr, t: Expr, e: Expr) extends Expr
  case class Eq(e1: Expr, e2: Expr) extends Expr
  case class Var(x: String) extends Expr
  case class Def(x: String, e: Expr, in: Expr) extends Expr
  case class Abs(x: String, e: Expr) extends Expr
  case class App(f: Expr, e: Expr) extends Expr
  
  /** Substitute the variable n by the expression e in expression in. */
  def subst (n: String, e: Expr, in: Expr): Expr = in match {
    case BinOp(op, e1, e2) => BinOp(op, subst(n, e, e1), subst(n, e, e2))
    case UnOp(op, e1) => UnOp(op, subst(n, e, e1))
    case Ite(c, e1, e2) => Ite(subst(n, e, c), subst(n, e, e1), subst(n, e, e2))
    case Eq(e1, e2) => Eq(subst(n, e, e1), subst(n, e, e2))
    case Def(x, e1, in) => if (x == n) Def(x, subst(n, e, e1), in) else Def(x, subst(n, e, e1), subst(n, e, in))
    case Abs(x, e1) => if (x == n) Abs(x, e1) else Abs(x, subst(n, e, e1))
    case App(f, e1) => App(subst(n, e, e1), subst(n, e, e2))
    case Var(x) => if (x == n) e else Var(x)
    case _ => in
  }
  
  /** Evaluates expression e in environment env. The order of evaluation
    * of the function calls is strict.
    */
  def eval(e: Expr, env: Map[String, Expr]): Expr = e match {
    case BinOp(o, e1, e2) => (eval(e1, env), eval(e2, env)) match {
      case (Val(v1), Val(v2)) => o match {
        case Plus  => Val(v1 + v2)
        case Minus => Val(v1 - v2)
        case Mult  => Val(v1 * v2)
        case Div   => Val(v1 / v2)
      }
      case (Val(v1), ee) => throw new NotEvaluable(ee+" is not a value.")
      case (ee, _)       => throw new NotEvaluable(ee+" is not a value.")
    }
    case UnOp(o, e1) => o match {
      case Plus => eval(e1, env)
      case Minus => eval(e1, env) match {
        case Val(v) => Val(-v)
        case ee     => throw new NotEvaluable(ee+" is not a value.")
      }
      case _ => throw new NotEvaluable("Syntax error: illegal unary operator "+o)
    }
    case Val(v)  => Val(v)
    case Bool(b) => Bool(b)
    case Ite(c, t, e) => eval(c, env) match {
      case Bool(True)  => eval(t, env)
      case Bool(False) => eval(e, env)
      case ce          => throw new NotEvaluable(ce+" is not a boolean")
    }
    case Eq(e1, e2) => (eval(e1, env), eval(e2, env)) match {
      case (Bool(b1), Bool(b2)) => Bool(if (b1 == b2) True else False)
      case (Val(v1), Val(v2))   => Bool(if (v1 == v2) True else False)
      case (ee1, ee2)           => throw new NotEvaluable(ee1+" and "+ee2+" are not comparable")
    }
    case Var(x) => try eval(env(x), env) catch {
      case ex: NoSuchElementException => throw new NotEvaluable("Undefined name "+x)
    }
    case Def(x, e1, in)      => eval(in, env + (x -> eval(e1, env)))
    case Abs(x, e)           => Abs(x, e)
    case App(e1, e2)         => eval(e1, env) match {
      case Abs(x, ee1) => eval(subst(x, eval(e2, env), ee1), env) /* Beta-réduction */
      case ee1 => throw new NotEvaluable(ee1+" is not a function, it cannot be applied.")
    }
  }

  /** Evaluates an expression to a value or fails with a NotEvaluable exception. */
  override def evaluate(e: Expr): Expr = eval(e, Map())

  /* Expressions for tests. */
  val e1 = BinOp(Plus, Val(2), Val(3))
  val e2 = BinOp(Mult, Val(2), e1)
  val e3 = UnOp(Minus, e2)
  val e4 = BinOp(Minus, e2, Var("x"))
  val e5 = Def("x", e1, e4)
  val e6 = Eq(e1, e5)
  val e7 = Ite(e6, e2, e3)
  val id = Abs("x", Var("x"))
  val t = Abs("t",Abs("f",Var("t")))
  val f = Abs("t",Abs("f",Var("f")))
  val f2 = App(App(f, e1), e2)
  val un = Abs("s", Abs("z", App(Var("s"), Var("z"))))
  val deux = Abs("s", Abs("z", App(Var("s"), App(Var("s"), Var("z")))))
  val succ = Abs("n",Abs("s", Abs("z", App(Var("s"), App(App(Var("n"), Var("s")), Var("z"))))))
  val truc = Def("t",t,App(Var("t"),Var("t")))
  val truc2 = Def("id", id, App(App(id, id), App(id,Val(1))))

  /** Tests the eval method. Should return true. */
  def test =
    eval(e1, Map()) == Val(5) &&
      eval(e2, Map()) == Val(10) &&
      eval(e3, Map()) == Val(-10) &&
      eval(e4, Map()) == BinOp(Minus, Val(10), Var("x")) &&
      eval(e4, Map("x" -> Val(10))) == Val(0) &&
      eval(e5, Map("x" -> Val(10))) == Val(5) &&
      eval(e6, Map()) == Bool(True) &&
      eval(e7, Map()) == Val(10)
}
*/