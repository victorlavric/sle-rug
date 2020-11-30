module Syntax

extend lang::std::Layout;
extend lang::std::Id;

/*
 * Concrete syntax of QL
 */

start syntax Form 
  = "form" Id name "{" Question* questions "}"; 

// TODO: question, computed question, block, if-then-else, if-then
syntax Question
  = Str Id name ":" Type ("=" Expr)?
  | "if" "(" Expr guard ")" "{" Question* questions "}" ("else" "{" Question* questions "}")?
  ;

// TODO: +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)
syntax Expr 
  = Id \ "true" \ "false" | Str | Int | Bool // true/false are reserved keywords.
  | "(" Expr ")"
  > "!" Expr
  > left(Expr lhs "*" Expr rhs | Expr lhs "/" Expr rhs)
  > left(Expr lhs "+" Expr rhs | Expr lhs "-" Expr rhs)
  > left(Expr lhs "\>=" Expr rhs | Expr lhs "\<=" Expr rhs | Expr lhs "\>" Expr rhs | Expr lhs "\<" Expr rhs)
  > left(Expr lhs "==" Expr rhs | Expr lhs "!=" Expr rhs)
  > left Expr lhs "&&" Expr rhs
  > left Expr lhs "||" Expr rhs 
  ;

syntax Type
  = "boolean" | "integer" | "str";  
  
lexical Str = [\"]![\"]*[\"];

lexical Int 
  = [1-9][0-9]* | [0];

lexical Bool 
  = "true" | "false";


