module AST

/*
 * Define Abstract Syntax for QL
 *
 * - complete the following data types
 * - make sure there is an almost one-to-one correspondence with the grammar
 */

data AForm(loc src = |tmp:///|)
  = form(str name, list[AQuestion] questions)
  ; 

data AQuestion(loc src = |tmp:///|)
  = question(str content, AId identifier, AType typeOf, list[AExpr] express)
  | blockQ(AExpr guard, list[AQuestion] ifs, list[AQuestion] elses)
  ;

data AExpr(loc src = |tmp:///|)
  = ref(AId id) | Str(str valStr) | Int(int valInt) | Bool(bool valBool)
  | par(AExpr expr)
  | not(AExpr expr)
  | plus(AExpr lhs, AExpr rhs)
  | minus(AExpr lhs, AExpr rhs)
  | mult(AExpr lhs, AExpr rhs)
  | div(AExpr lhs, AExpr rhs)
  | greaterEq(AExpr lhs, AExpr rhs)
  | smallerEq(AExpr lhs, AExpr rhs)
  | greater(AExpr lhs, AExpr rhs)
  | smaller(AExpr lhs, AExpr rhs)
  | equal(AExpr lhs, AExpr rhs)
  | notEq(AExpr lhs, AExpr rhs)
  | and(AExpr lhs, AExpr rhs)
  | or(AExpr lhs, AExpr rhs)
  ;

data AId(loc src = |tmp:///|)
  = id(str name);

data AType(loc src = |tmp:///|)
  = \type(str typeOf)
  ;