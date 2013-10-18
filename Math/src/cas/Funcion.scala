package cas

/*
 * Funcion.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */


import scala.util.parsing.combinator._
import numeros._
import estructuras._
import estructuras.vector._;
import excepciones.NoTieneInversoException

sealed abstract class Expr extends Cuerpo[Expr] {
	def esCero :Boolean = this==Expr.Cero;
    def esUno  :Boolean = this==Expr.Uno;

    def +(that: Expr) :Expr = BinOp("+",this,that);
    def -(that: Expr) :Expr = BinOp("-",this,that);
    def unary_-       :Expr = UnOp("-",this);
    def *(that: Expr) :Expr = BinOp("*",this,that);
    def /(that: Expr) :Expr = BinOp("/",this,that);
    def inverso       :Expr = if (esCero) throw new NoTieneInversoException(); else BinOp("/",UnoExpr,this);

    def cero          :Expr = Expr.Cero;
    def uno           :Expr = Expr.Uno;

    def get           :Expr = this match {
      	case NumComplejo(x) => new NumComplejo(x.get)
     	case NumReal(x) 	=> new NumReal(x.get)
     	case NumRacional(x) => new NumRacional(x.get)
     	case CeroExpr		=> CeroExpr
     	case UnoExpr		=> UnoExpr
     	case Var(x) 		=> new Var(x)
     	case BinOp(op,l,r) 	=> new BinOp(op,l.get,r.get)
     	case UnOp(op,l) 	=> new UnOp(op,l.get)
     	case Fun(n,e) 		=> new Fun(n,e.get)
    }

    def conjugado     :Expr = this match {
      case NumComplejo(x) 	=> NumComplejo(x.conjugado);
      case BinOp(op,e1,e2) 	=> BinOp(op,e1.conjugado,e2.conjugado);
      case UnOp(op,e)		=> UnOp(op,e.conjugado);
      case e :Expr			=> e;
    }
    def Re        	  :Real = null;

    override def toString :String = {
     def binPrec(op: String) = op match {
    	case "+" => 1
    	case "-" => 1
    	case "*" => 2
    	case "/" => 2
    	case "^" => 3
    	case _ => error("Unknown binary operator: " + op)
     }
     def unPrec = 4

     def precedence(e: Expr) = e match {
     	case BinOp(op, _, _) 	=> binPrec(op)
    	case _					=> unPrec
     }

     def combineBinary(op: String, a: Expr, b: Expr) = {
    	val aPrec = precedence(a)
    	val bPrec = precedence(b)
    	val opPrec = binPrec(op)
    	var aStr = a.toString;
    	var bStr = b.toString;
    	if (aPrec < opPrec) aStr = "(" + aStr + ")"
    	if (bPrec < opPrec) bStr = "(" + bStr + ")"
    	aStr + " " + op + " " + bStr
     }

     def combineUnary(op: String, e: Expr) = {
    	val eStr = e.toString;
    	op + (if (precedence(e) < unPrec) "(" + eStr + ")" else eStr)
     }

     this.simplifica match {
       	case NumComplejo(x) => x.toString
     	case NumReal(x) 	=> x.toString
     	case NumRacional(x) => x.toString
     	case CeroExpr		=> "0"
     	case UnoExpr		=> "1"
     	case Var(x) 		=> x
     	case BinOp(op,l,r) 	=> combineBinary(op,l,r)
     	case UnOp(op,l) 	=> combineUnary(op,l)
     	case Fun(n,e) 		=> n + "(" + e.toString + ")"
     }
   }

    private def rellena(x :Kn[Real]) :Map[String,Any] = {
        var mapa0 = Map[String,Real]();
        for(i<-0 until x.dim) {
            i match {
                case 0 =>mapa0=mapa0+("x"->x(i));
                case 1 =>mapa0=mapa0+("y"->x(i));
                case 2 =>mapa0=mapa0+("z"->x(i));
                case _ =>
            }
            mapa0=mapa0+("x"+i.toString->x(i));
        }
        mapa0;
    }

    def apply(vector :Kn[Real]) :Real = evalReal(rellena(vector));
    def apply(z :Complejo) :Complejo =evalComplejo(Map("z"->z));

    def evalRacional(mapa :Map[String,Any]) :Racional[Entero] = {
      this match {
        case NumComplejo(_)		=> throw new ErrorTipo("La expresion es compleja, utilizar evalComplejo");
        case NumReal(x) 		=> throw new ErrorTipo("La expresion es real, utilizar evalReal");
        case NumRacional(x)		=> x;
        case CeroExpr			=> Racional.Cero;
        case UnoExpr			=> Racional.Uno;
        case Fun(n,expresion) 	=> throw new ErrorTipo("Se ha intentado utilizar una funcion real, utilizar evalReal")
        case BinOp(op,l,r)		=> op match {
          								case "+" => l.evalRacional(mapa)+r.evalRacional(mapa);
          								case "-" => l.evalRacional(mapa)-r.evalRacional(mapa);
          								case "*" => l.evalRacional(mapa)*r.evalRacional(mapa);
          								case "/" => l.evalRacional(mapa)/r.evalRacional(mapa);
          								case "^" => throw new ErrorTipo("Se ha intentado utilizar la exponenciacion real, utilizar evalReal")
        							}
        case UnOp(op,e)			=> op  match {
          								case "+" => e.evalRacional(mapa);
          								case "-" => -e.evalRacional(mapa);
        							}
        case Var(s)				=> mapa(s).asInstanceOf[Racional[Entero]];
        }
    }

    def evalReal(mapa :Map[String,Any]) :Real = {
      this match {
        case NumComplejo(_)		=> throw new ErrorTipo("La expresion es compleja, utilizar evalComplejo");
        case NumReal(x) 		=> x
        case NumRacional(x)		=> x;
        case CeroExpr			=> Real.Cero;
        case UnoExpr			=> Real.Uno;
        case Fun(n,expresion) 	=> n match {
          								case "sin"|"sen" 	=> expresion.evalReal(mapa).sen;
          								case "cos" 			=> expresion.evalReal(mapa).cos;
          								case "tan"|"tg" 	=> expresion.evalReal(mapa).tan;
          								case "log" 			=> expresion.evalReal(mapa).log;
          								case "exp" 			=> expresion.evalReal(mapa).exp;
          								case "sqrt"			=> expresion.evalReal(mapa).sqrt;
        							}
        case BinOp(op,l,r)		=> op match {
          								case "+" => l.evalReal(mapa)+r.evalReal(mapa);
          								case "-" => l.evalReal(mapa)-r.evalReal(mapa);
          								case "*" => l.evalReal(mapa)*r.evalReal(mapa);
          								case "/" => l.evalReal(mapa)/r.evalReal(mapa);
          								case "^" => (l.evalReal(mapa).log*r.evalReal(mapa)).exp
        							}
        case UnOp(op,e)			=> op  match {
          								case "+" => e.evalReal(mapa);
          								case "-" => -e.evalReal(mapa);
        							}
        case Var(s)				=> mapa(s).asInstanceOf[Real];
        }
    }

    def evalComplejo(mapa :Map[String,Any]) :Complejo = {
      this match {
        case NumComplejo(x)		=> x;
        case NumReal(x) 		=> x;
        case NumRacional(x)		=> x;
        case CeroExpr			=> Complejo.Cero;
        case UnoExpr			=> Complejo.Uno;
        case Fun(n,expresion) 	=> n match {
          								case "sin"|"sen" 	=> expresion.evalReal(mapa).sen;
          								case "cos" 			=> expresion.evalReal(mapa).cos;
          								case "tan"|"tg" 	=> expresion.evalReal(mapa).tan;
          								case "log" 			=> expresion.evalReal(mapa).log;
          								case "exp" 			=> expresion.evalReal(mapa).exp;
          								case "sqrt"			=> expresion.evalReal(mapa).sqrt;
        							}
        case BinOp(op,l,r)		=> op match {
          								case "+" => l.evalComplejo(mapa)+r.evalComplejo(mapa);
          								case "-" => l.evalComplejo(mapa)-r.evalComplejo(mapa);
          								case "*" => l.evalComplejo(mapa)*r.evalComplejo(mapa);
          								case "/" => l.evalComplejo(mapa)/r.evalComplejo(mapa);
          								case "^" => (l.evalComplejo(mapa).log*r.evalComplejo(mapa)).exp
        							}
        case UnOp(op,e)			=> op  match {
          								case "+" => e.evalComplejo(mapa);
          								case "-" => -e.evalComplejo(mapa);
        							}
        case Var(s)				=> mapa(s).asInstanceOf[Complejo];
        }
    }

    def D(Variable :String) : Expr = (this match {
    	case NumReal(x) 					=> CeroExpr
    	case Var(Variable)					=> UnoExpr
    	case Var(_)							=> CeroExpr
    	case UnOp("-",u) 					=> UnOp("-",u.D(Variable))
    	case BinOp("+", u, v) 				=> BinOp("+", u.D(Variable), v.D(Variable))
    	case BinOp("-", u, v) 				=> BinOp("-", u.D(Variable), v.D(Variable))
    	case BinOp("*", NumReal(x),u)		=> BinOp("*",NumReal(x),u.D(Variable))
    	case BinOp("*", u,NumReal(x))		=> BinOp("*",NumReal(x),u.D(Variable))
    	case BinOp("*", u, v) 				=> BinOp("+", BinOp("*", u, v.D(Variable)),BinOp("*", v, u.D(Variable)))
    	case BinOp("/", u, v)				=> BinOp("/",BinOp("-",BinOp("*",v,u.D(Variable)),BinOp("*",u,v.D(Variable))),BinOp("^",v,NumReal(Real(2))))
    	case BinOp("^",u,NumReal(x))		=> BinOp("*",BinOp("*",NumReal(x),BinOp("^",u,BinOp("-",NumReal(x),NumReal(Real.Uno)))),u.D(Variable))
    	case BinOp("^", u, v)				=> BinOp("+",BinOp("*",BinOp("*",v,BinOp("^",u,BinOp("-",v,NumReal(Real.Uno)))),u.D(Variable)),BinOp("*",BinOp("*",BinOp("^",u,v),v.D(Variable)),Fun("log",u)))
    	case Fun("sin" | "sen", u)			=> BinOp("*", Fun("cos", u), u.D(Variable))
    	case Fun("cos", u) 					=> BinOp("*", UnOp("-",Fun("sin", u)), u.D(Variable))
    	case Fun("log" ,u)					=> BinOp("/", u.D(Variable),u)
    	case Fun("exp" ,u)					=> BinOp("*", Fun("exp",u),u.D(Variable))
    	case Fun("tan" | "tg", u)			=> BinOp("/", u.D(Variable),BinOp("^",Fun("cos",u),NumReal(Real(2))))
    }).simplifica;

    def simplifica : Expr = this match {
    	case Fun(s, e) 							=> Fun(s, e.simplifica)
    	case UnOp("+", e) 						=> e.simplifica
    	case UnOp("-", NumReal(n)) 				=> NumReal(-n)
    	case UnOp("-", UnOp("-", e)) 			=> e.simplifica
    	case UnOp("-", e) 						=> UnOp("-", e.simplifica)
    	case BinOp("+", NumReal(x),NumReal(y))  => NumReal(x+y)
    	case BinOp("+", CeroExpr, e)		 	=> e.simplifica
    	case BinOp("+", e, CeroExpr)		 	=> e.simplifica
    	case BinOp("-", NumReal(x),NumReal(y))  => NumReal(x-y)
    	case BinOp("-", e, CeroExpr)			=> e.simplifica
    	case BinOp("-", CeroExpr, e)		 	=> UnOp("-", e.simplifica)
    	case BinOp("*", NumReal(x),NumReal(y))  => NumReal(x*y)
    	case BinOp("*", CeroExpr, e)		 	=> CeroExpr;
    	case BinOp("*", e, CeroExpr)			=> CeroExpr
    	case BinOp("*", UnoExpr, e)			 	=> e.simplifica
    	case BinOp("*", e, UnoExpr)				=> e.simplifica
    	case BinOp("*", BinOp("^",e,NumReal(y)),BinOp("^",e2,NumReal(y2))) if e==e2 => BinOp("^",e.simplifica,NumReal(y+y2))
    	case BinOp("/", NumReal(x),NumReal(y))  => NumReal(x/y)
    	case BinOp("/", e, UnoExpr)			 	=> e.simplifica
    	case BinOp("/", BinOp("^",e,NumReal(y)),BinOp("^",e2,NumReal(y2))) if e==e2 => BinOp("^",e.simplifica,NumReal(y-y2))
    	case BinOp("+", BinOp("*", NumReal(n), e),BinOp("*", NumReal(m), e2)) if e==e2 =>BinOp("*", NumReal(n+m), e.simplifica)
    	//case BinOp("+", BinOp("^", Fun("sin" | "sen" , _), NumReal(2)),BinOp("^", Fun("cos", _), NumReal(2))) => UnoExpr
    	case BinOp("^", e, UnoExpr)				=> e.simplifica;
    	case BinOp("^", _, CeroExpr)			=> UnoExpr
    	case BinOp(op, l, r) 					=> BinOp(op, l.simplifica,r.simplifica)
    	case x :Expr 							=> x
    }

}

case class NumComplejo(num :Complejo) extends Expr

case class NumReal(num: Real) extends Expr

case class NumRacional(num :Racional[Entero]) extends Expr

//case class NumEntero(num :Entero) extends Expr
case class UnOp(operator: String, operand : Expr) extends Expr

case class BinOp(operator: String,Left: Expr, Right: Expr) extends Expr

case class Fun(name: String, arg: Expr) extends Expr

case class Var(name :String) extends Expr

case object CeroExpr extends Expr;

case object UnoExpr extends Expr;

object Expr {
  implicit val Cero :Expr = CeroExpr;
  val Uno  :Expr = UnoExpr;

  implicit def str2Expr(s :String) 					:Expr = {
    val f=new Funcion(List(s));
    f.fnc(0);
  }
  implicit def double2Expr(x :Double) 				:Expr = NumReal(x);
  implicit def real2Expr(x :Real) 					:Expr = NumComplejo(x);
  implicit def racional2Expr(x :Racional[Entero]) 	:Expr = NumRacional(x);
}

case class BinOpList(operator: String,Left: List[Expr], Right: List[Expr]) extends Expr

class ErrorTipo(s :String) extends Exception(s);

class Funcion(var funciones :List[String]) extends JavaTokenParsers with RunParser{

	var tipo = -1;

	private val f = (a :Expr,b :String~Expr)=>b match {case operacion~sum => BinOp(operacion,a,sum)}

	lazy val sumExpr :Parser[Expr]= prodExpr ~ rep("+" ~ prodExpr | "-" ~ prodExpr ) ^^
                       {case seed ~ fs => fs.foldLeft(seed)(f)}
    lazy val prodExpr :Parser[Expr]= expExpr ~ rep("*" ~ expExpr | "/" ~ expExpr ) ^^
                        {case seed ~ fs => fs.foldLeft(seed)(f)}
    lazy val expExpr :Parser[Expr] = factor ~ rep("^"~factor) ^^
    					{case seed ~ fs => fs.foldLeft(seed)(f)}

    lazy val factor :Parser[Expr] = wholeNumber ~ "/" ~ wholeNumber							^^{case x ~ "/" ~ y => if (tipo<1) tipo=1; val x2=NumRacional(Racional(Entero(x.toInt),Entero(y.toInt))); if (x2.esCero) CeroExpr else if (x2.esUno) UnoExpr; else x2;} |
    								floatingPointNumber ~ "+" ~ floatingPointNumber <~ "i"	^^{case x ~ "+" ~ y=> if (tipo<3) tipo=3; val x2=NumComplejo(Complejo(x.toDouble,y.toDouble)); if (x2.esCero) CeroExpr else if (x2.esUno) UnoExpr; else x2;} |
    								floatingPointNumber <~ "i"								^^{x=> if (tipo<3) tipo=3; val x2=NumComplejo(Complejo(0,x.toDouble)); if (x2.esCero) CeroExpr else x2;} |
    								floatingPointNumber 									^^{x=> if (tipo<2) tipo=2; val x2=NumReal(Real(x.toDouble)); if (x2.esCero) CeroExpr else if (x2.esUno) UnoExpr; else x2;} |
    								"(" ~> sumExpr <~ ")" |
    							    "-" ~> expExpr			 								^^{case x :Expr => UnOp("-",x)}  |
    							    "+" ~> expExpr											^^{case x :Expr => UnOp("+",x)}  |
    							    funName |
    							    "pi"													^^^{if (tipo<2) tipo=2; NumReal(Real(Math.PI))} |
                                    "e"														^^^{if (tipo<2) tipo=2; NumReal(Real(Math.E))} |
                                    ident													^^{case s :String => Var(s)};
    lazy val funName = ("sin" | "sen" | "cos" | "tan" | "tg" | "exp" | "log" | "sqrt") ~ "(" ~ sumExpr ~ ")" ^^{case op ~ "(" ~ x ~ ")" => if (tipo<2) tipo=2; Fun(op,x)}

    type RootType=Expr;

    var fnc = funciones.par.map(run).map(_.get).map(_.simplifica).toList;

    def root = sumExpr;

   def agrupa(e :Expr) :Expr= {
   		def lado(e :Expr,operador :String) :List[Expr] = {
   			var resultado :List[Expr] = Nil;
	   		def seguidas(e :Expr,Operador :String,mono :Boolean) {
	   		  e match {
	   		  	case BinOp(Operador,NumReal(x),r) 	=> resultado=NumReal(x)::resultado;
	   		  										   seguidas(r,Operador,true);
	   		  	case BinOp(Operador,Fun(s,x),r)		=> resultado=Fun(s,x)::resultado;
	   		  										   seguidas(r,Operador,true);
	   		  	case BinOp(Operador,Var(s),r)		=> resultado=Var(s)::resultado;
	   		  										   seguidas(r,Operador,true);
	   		  	case BinOp(Operador,l,NumReal(x)) 	=> resultado=NumReal(x)::resultado;
	   		  										   seguidas(l,Operador,true);
	   		  	case BinOp(Operador,l,Fun(s,x))		=> resultado=Fun(s,x)::resultado;
	   		  										   seguidas(l,Operador,true);
	   		  	case BinOp(Operador,l,Var(s))		=> resultado=Var(s)::resultado;
	   		  										   seguidas(l,Operador,true);
	   		  	case BinOp(Operador,l,r)			=> seguidas(l,Operador,true);
	   		  										   seguidas(r,Operador,true);
	   		  	case e if mono						=> resultado=e::resultado
	   		  }
	   		}
	   		seguidas(e,operador,false);
	   		resultado;
   		}
   		e match {
   		  //case BinOp("+",BinOp("*",l1,r1),e2) => BinOpList("+",lado(BinOp("*",agrupa(l1),agrupa(r1)),"*"),agrupa(e2));
   		  case BinOp("+",e1,e2) => BinOp("+",agrupa(e1),agrupa(e2));
   		  case e => e
   		}
   }
}
