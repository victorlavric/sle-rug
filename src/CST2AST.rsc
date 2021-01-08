module CST2AST

import Syntax;
import AST;

import ParseTree;
import String;

/*
 * Implement a mapping from concrete syntax trees (CSTs) to abstract syntax trees (ASTs)
 *
 * - Use switch to do case distinction with concrete patterns (like in Hack your JS) 
 * - Map regular CST arguments (e.g., *, +, ?) to lists 
 *   (NB: you can iterate over * / + arguments using `<-` in comprehensions or for-loops).
 * - Map lexical nodes to Rascal primitive types (bool, int, str)
 * - See the ref example on how to obtain and propagate source locations.
 */

AForm cst2ast(start[Form] sf) {
  Form f = sf.top; // remove layout before and after form
  return form("<f.name>", [cst2ast(q) | q <- f.questions], src=f@\loc); 
}

AQuestion cst2ast(Question q) {
	switch (q) {
		case (Question)`<Str x> <Id y> : <Type z>`: return question("<x>", id("<y>", src=y@\loc), cst2ast(z), [], src=q@\loc);
		case (Question)`<Str x1> <Id y1> : <Type z1>  = <Expr expression>`: 
			return question("<x1>", id("<y1>", src=y1@\loc), cst2ast(z1), [cst2ast(expression)], src=q@\loc);
		case (Question)`if (<Expr guard>) { <Question* questions>} else {<Question* elsesQ>}`: 
			return blockQ(cst2ast(q.guard), ([] | it + cst2ast(qq) | qq <- questions), ([] | it + cst2ast(qq) | qq <- elsesQ), src=q@\loc);
		case Question q:
			return blockQ(cst2ast(q.guard), [cst2ast(qs) | qs <- q.questions], [], src=q@\loc);
	}
}

AExpr cst2ast(Expr e) {
  switch (e) {
    case (Expr)`<Id x>`: return ref(id("<x>", src=x@\loc), src=x@\loc);
    case (Expr)`<Str x>`: return Str("<x>", src=x@\loc);
    case (Expr)`<Int x>`: return Int(toInt("<x>"));
    case (Expr)`<Bool x>`: return Bool((Bool)`true` := x ? true : false);
    case (Expr)`(<Expr expression>)`: return par(cst2ast(expression), src=e@\loc);
    case (Expr)`!<Expr expression>`: return not(cst2ast(expression), src=e@\loc);    
	case (Expr)`<Expr lhs> + <Expr rhs>`: return plus(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
	case (Expr)`<Expr lhs> - <Expr rhs>`: return minus(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
	case (Expr)`<Expr lhs> * <Expr rhs>`: return mult(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
	case (Expr)`<Expr lhs> / <Expr rhs>`: return div(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
	case (Expr)`<Expr lhs> \>= <Expr rhs>`: return greaterEq(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
	case (Expr)`<Expr lhs> \<= <Expr rhs>`: return smallerEq(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
	case (Expr)`<Expr lhs> \> <Expr rhs>`: return greater(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
	case (Expr)`<Expr lhs> \< <Expr rhs>`: return smaller(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
	case (Expr)`<Expr lhs> && <Expr rhs>`: return and(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
	case (Expr)`<Expr lhs> || <Expr rhs>`: return or(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
	case (Expr)`<Expr lhs> == <Expr rhs>`: return equal(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
	case (Expr)`<Expr lhs> != <Expr rhs>`: return notEq(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    default: throw "Unhandled expression: <e>";
  }
}

AType cst2ast(Type t) {
  if (t := (Type)`boolean` || t := (Type)`integer` || t:=(Type)`str`)
  	return \type("<t>", src=t@\loc);
}
