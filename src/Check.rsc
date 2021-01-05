module Check

import AST;
import Resolve;
import Message; // see standard library
import IO;
import List;

data Type
  = tint()
  | tbool()
  | tstr()
  | tunknown()
  ;

// the type environment consisting of defined questions in the form 
alias TEnv = rel[loc def, str name, str label, Type \type];

// To avoid recursively traversing the form, use the `visit` construct
// or deep match (e.g., `for (/question(...) := f) {...}` ) 
TEnv collect(AForm f) {
  TEnv collection = {};
  visit (f) {
  	case q:question(str content, AId identifier, AType typeOf, list[AExpr] express): {
  			if (typeOf.typeOf == "boolean")
	  			collection += {<identifier.src, content, identifier.name, tbool()>};
	  		else if (typeOf.typeOf == "integer")
	  			collection += {<identifier.src, content, identifier.name, tint()>};
	  		else if (typeOf.typeOf == "str")
	  			collection += {<identifier.src, content, identifier.name, tstr()>};
	  		else
	  			collection += {<identifier.src, content, identifier.name, tunknown()>};
  		}
  }
  return collection; 
}

set[Message] check(AForm f, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  for (q <- f.questions)
  	msgs += check(q, tenv, useDef);

  return msgs; 
}

// - produce an error if there are declared questions with the same name but different types.
// - duplicate labels should trigger a warning 
// - the declared type computed questions should match the type of the expression.
set[Message] check(AQuestion q, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  
  if (blockQ(AExpr guard, list[AQuestion] ifs, list[AQuestion] elses):=q) {
  	msgs += check(guard, tenv, useDef);
    if (typeOf(guard, tenv, useDef) != tbool())
	  msgs += {error("Guard in if-else statement must be boolean", guard.src)};
  	for (qq <- ifs + elses)
  		msgs += check(qq, tenv, useDef);
  	return msgs;
  }
  // Same label stuff.
  if (size([t.label | t <- tenv, t.label == q.identifier.name]) > 1)
 	  msgs += {warning("Two questions have the same label.", q.identifier.src)};
  
  // Determine the type.
  Type t = tunknown();
  if (q.typeOf.typeOf == "boolean")
	t = tbool();
  else if (q.typeOf.typeOf == "integer")
    t = tint();
  else if (q.typeOf.typeOf == "str")
    t = tstr();
  
  // Checking for same names and different types.
  for (tv <- tenv) {
    if (tv.label == q.identifier.name && t != tv.\type)
      msgs += {error("There are multiple declarations of questions with the same label, though different types.", q.identifier.src)};
  	if (tv.name == q.content && tv.label != q.identifier.name) {
  	  msgs += {warning("Same questions have different labels", q.src)};
  	  }
  }
  
  
  // Checking for the same type of the expression and type of the question.
  for (expr <- q.express) {
  	if (typeOf(expr, tenv, useDef) != t)
      msgs += {error("The question type and expression type do not match.", q.typeOf.src)};
    msgs += check(expr, tenv, useDef);
  }
      
  return msgs; 
}

// Check operand compatibility with operators.
// E.g. for an addition node add(lhs, rhs), 
//   the requirement is that typeOf(lhs) == typeOf(rhs) == tint()
set[Message] check(AExpr e, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  switch (e) {
    case ref(AId x):
      msgs += { error("Undeclared question", x.src) | useDef[x.src] == {} };
	
	case par(AExpr expr): 
	  msgs += { error("Unknown type between parantheses.", expr.src) | typeOf(expr, tenv, useDef) == tunknown() } + check(expr, tenv, useDef);
	
	case not(AExpr expr):
	  msgs += { error("You cannot negate a non boolean data type.", expr.src) | typeOf(expr, tenv, useDef) != tbool() } + check(expr, tenv, useDef);
	
	case expr:plus(AExpr lhs, AExpr rhs):
	  msgs += { error("You cannot perform the addition of two types that are not integers.", expr.src) | typeOf(lhs, tenv, useDef) != tint() } 
	            + check(lhs, tenv, useDef) + check(rhs, tenv, useDef);
	
	case minus(AExpr lhs, AExpr rhs):
	  msgs += { error("You cannot perform the difference of two types that are not integers.", e.src) | typeOf(e, tenv, useDef) != tint() } 
	             + check(lhs, tenv, useDef) + check(rhs, tenv, useDef); 
	
	case mult(AExpr lhs, AExpr rhs):
	  msgs += { error("You cannot perform the multiplication of two types that are not integers.", e.src) | typeOf(e, tenv, useDef) != tint() }
	            + check(lhs, tenv, useDef) + check(rhs, tenv, useDef);
	
	case div(AExpr lhs, AExpr rhs):
	  msgs += { error("You cannot perform the division of two types that are not integers.", e.src) | typeOf(e, tenv, useDef) != tint() } 
	            + check(lhs, tenv, useDef) + check(rhs, tenv, useDef);
	
	case and(AExpr lhs, AExpr rhs):
	  msgs += { error("You cannot use conjunction if either of questions has a non boolean data type.", e.src) | typeOf(e, tenv, useDef) != tbool()} 
	              + check(lhs, tenv, useDef) + check(rhs, tenv, useDef);
  	
  	case or(AExpr lhs, AExpr rhs):
  	  msgs += { error("You cannot use disjunction if either of questions has a non boolean data type.", e.src) | typeOf(e, tenv, useDef) != tbool()} 
  	            + check(lhs, tenv, useDef) + check(rhs, tenv, useDef);
  	
  	default:
  		if (greaterEq(AExpr lhs, AExpr rhs) := e || smallerEq(AExpr lhs, AExpr rhs) := e || greater(AExpr lhs, AExpr rhs) := e ||
  			smaller(AExpr lhs, AExpr rhs) := e || equal(AExpr lhs, AExpr rhs) := e || notEq(AExpr lhs, AExpr rhs) := e) {
  		  msgs += { error("You cannot compare these two types.", e.src) | typeOf(e, tenv, useDef) == tunknown()} 
  		            + check(lhs, tenv, useDef) + check(rhs, tenv, useDef);
  		}
  }	
  
  return msgs; 
}

Type typeOf(AExpr e, TEnv tenv, UseDef useDef) {
  switch (e) {
    case ref(id(_, src = loc u)):
      if (<u, loc d> <- useDef, <d, x, _, Type t> <- tenv) {
        return t;
      }
     case Str(str _):
     	return tstr();
     case Int(int _):
     	return tint();
     case Bool(bool _): {
     	return tbool();
     }
     case par(AExpr expr): {
     	Type t = typeOf(expr, tenv, useDef);
     	if (t != tunknown())
     		return t;
 	 }
     case not(AExpr expr): {
     	Type t = typeOf(expr, tenv, useDef);
     	if (t == tbool())
     		return tbool();
     }
     case plus(AExpr lhs, AExpr rhs): return isInteger(lhs, rhs, tenv, useDef);
     case minus(AExpr lhs, AExpr rhs): return isInteger(lhs, rhs, tenv, useDef);
     case mult(AExpr lhs, AExpr rhs): return isInteger(lhs, rhs, tenv, useDef);
     case div(AExpr lhs, AExpr rhs): return isInteger(lhs, rhs, tenv, useDef);
     case greaterEq(AExpr lhs, AExpr rhs): return isNotUnknown(lhs, rhs, tenv, useDef);
     case smallerEq(AExpr lhs, AExpr rhs): return isNotUnknown(lhs, rhs, tenv, useDef);
     case greater(AExpr lhs, AExpr rhs): return isNotUnknown(lhs, rhs, tenv, useDef);
     case smaller(AExpr lhs, AExpr rhs): return isNotUnknown(lhs, rhs, tenv, useDef);
     case equal(AExpr lhs, AExpr rhs): return isNotUnknown(lhs, rhs, tenv, useDef);
     case notEq(AExpr lhs, AExpr rhs): return isNotUnknown(lhs, rhs, tenv, useDef);
     case and(AExpr lhs, AExpr rhs): return isBool(lhs, rhs, tenv, useDef);
     case or(AExpr lhs, AExpr rhs): return isBool(lhs, rhs, tenv, useDef);
  }
  return tunknown(); 
}


Type isInteger(AExpr lhs, AExpr rhs, TEnv tenv, UseDef useDef) {
  Type t1 = typeOf(lhs, tenv, useDef);
  Type t2 = typeOf(rhs, tenv, useDef);
  if (t1 == t2 && t1 == tint())
  	return tint();
  return tunknown();
}

Type isNotUnknown(AExpr lhs, AExpr rhs, TEnv tenv, UseDef useDef) {
  Type t1 = typeOf(lhs, tenv, useDef);
  Type t2 = typeOf(rhs, tenv, useDef);
  if (t1 == t2 && t1 != tunknown())
  	return tbool();
  return tunknown();
}

Type isBool(AExpr lhs, AExpr rhs, TEnv tenv, UseDef useDef) {
  Type t1 = typeOf(lhs, tenv, useDef);
  Type t2 = typeOf(rhs, tenv, useDef);
  if (t1 == t2 && t1 == tbool())
  	return tbool();
  return tunknown();
}

/* 
 * Pattern-based dispatch style:
 * 
 * Type typeOf(ref(id(_, src = loc u)), TEnv tenv, UseDef useDef) = t
 *   when <u, loc d> <- useDef, <d, x, _, Type t> <- tenv
 *
 * ... etc.
 * 
 * default Type typeOf(AExpr _, TEnv _, UseDef _) = tunknown();
 *
 */
 
 

