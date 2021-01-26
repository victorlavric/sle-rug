module Eval

import AST;
import Resolve;
import IO;

/*
 * Implement big-step semantics for QL
 */
 
// NB: Eval may assume the form is type- and name-correct.


// Semantic domain for expressions (values)
data Value
  = vint(int n)
  | vbool(bool b)
  | vstr(str s)
  ;

// The value environment
alias VEnv = map[str name, Value \value];

// Modeling user input
data Input
  = input(str question, Value \value);
  
// produce an environment which for each question has a default value
// (e.g. 0 for int, "" for str etc.)
VEnv initialEnv(AForm f) {
  return (identifier.name: defaultFor(typeOf) | /question(str _, AId identifier, AType typeOf, list[AExpr] _) := f);
}

Value defaultFor(\type("boolean")) = vbool(false);
Value defaultFor(\type("integer")) = vint(0);
Value defaultFor(\type("str")) = vstr(""); 

// Because of out-of-order use and declaration of questions
// we use the solve primitive in Rascal to find the fixpoint of venv.
VEnv eval(AForm f, Input inp, VEnv venv) {
  if (input(str name, Value \value) := inp) {
    venv[name] = \value;
  }
  return solve (venv) {
    venv = evalOnce(f, inp, venv);
  }
}

VEnv evalOnce(AForm f, Input inp, VEnv venv) {
  
  for (quest <- f.questions)
  	venv = eval(quest, inp, venv);
  
  return venv;
}

VEnv eval(AQuestion q, Input inp, VEnv venv) {
  // evaluate conditions for branching,
  // evaluate inp and computed questions to return updated VEnv
  
  
  if (blockQ(AExpr guard, list[AQuestion] ifs, list[AQuestion] elses) := q) {
  	
  	if (deconstruct(eval(guard, venv)) == true) {
  		for (qq <- ifs)
  			venv = eval(qq, inp, venv);
  	}
  	else {
  		for (qq <- elses)
  			venv = eval(qq, inp, venv);
  	}
  	
  	return venv;
  }
  
  return eval(q, venv); 
}

VEnv eval(AQuestion q, VEnv venv) {
  for (expr <- q.express) {
    venv[q.identifier.name] = eval(expr, venv);
  }
		 
  return venv;
}

int deconstruct(vint(int v)) = v;
bool deconstruct(vbool(bool v)) = v;
str deconstruct(vstr(str v)) = v;

Value eval(AExpr e, VEnv venv) {
  switch (e) {
    case ref(AId x): return venv[x.name];
    case Str(str x): return vstr(x);
    case Int(int x): return vint(x);
    case Bool(bool x): return vbool(x);
    case par(AExpr expr): return eval(expr, venv);
    case not(AExpr expr): return vbool(!deconstruct(eval(expr, venv)));
    case plus(AExpr lhs, AExpr rhs): return vint(deconstruct(eval(lhs, venv)) + deconstruct(eval(rhs, venv))); 
    case minus(AExpr lhs, AExpr rhs): return vint(deconstruct(eval(lhs, venv)) - deconstruct(eval(rhs, venv)));
    case mult(AExpr lhs, AExpr rhs): return vint(deconstruct(eval(lhs, venv)) * deconstruct(eval(rhs, venv)));
    case div(AExpr lhs, AExpr rhs): {
    	int v_rhs = deconstruct(eval(rhs, venv));
    	if (v_rhs == 0)
    		throw "Unsupported operation: Division by 0.";
    	return vint(deconstruct(eval(lhs, venv)) / v_rhs);
    }
    case greaterEq(AExpr lhs, AExpr rhs): return vbool(deconstruct(eval(lhs, venv)) >= deconstruct(eval(rhs, venv)));
    case smallerEq(AExpr lhs, AExpr rhs): return vbool(deconstruct(eval(lhs, venv)) <= deconstruct(eval(rhs, venv)));
    case greater(AExpr lhs, AExpr rhs): return vbool(deconstruct(eval(lhs, venv)) > deconstruct(eval(rhs, venv))); 
    case smaller(AExpr lhs, AExpr rhs): return vbool(deconstruct(eval(lhs, venv)) < deconstruct(eval(rhs, venv)));
    case equal(AExpr lhs, AExpr rhs): return vbool(deconstruct(eval(lhs, venv)) == deconstruct(eval(rhs, venv)));
    case notEq(AExpr lhs, AExpr rhs): return vbool(deconstruct(eval(lhs, venv)) != deconstruct(eval(rhs, venv)));
    case and(AExpr lhs, AExpr rhs): return vbool(deconstruct(eval(lhs, venv)) && deconstruct(eval(rhs, venv)));
    case or(AExpr lhs, AExpr rhs): return vbool(deconstruct(eval(lhs, venv)) || deconstruct(eval(rhs, venv)));
    default: throw "Unsupported expression <e>";
  }
}