module Syntax

extend lang::std::Layout;
extend lang::std::Id;

/*
 * Concrete syntax of QL
 */

start syntax Form 
  = "form" Id "{" Question* "}"; 

// TODO: question, computed question, block, if-then-else, if-then
syntax Question
  = Str Id ":" Type ("=" Expr)?
  | "if" "(" Expr ")" "{" Question* "}" ("else" "{" Question* "}")?
  ;

// TODO: +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)
syntax Expr 
  = Id \ "true" \ "false" | Str | Int | Bool // true/false are reserved keywords.
  | [(] Expr [)]
  > [!] Expr
  > left(Expr [*] Expr | Expr [/] Expr)
  > left(Expr [+] Expr | Expr [\-] Expr)
  > left(Expr "\>=" Expr | Expr "\<=" Expr | Expr "\>" Expr | Expr "\<" Expr)
  > left(Expr "==" Expr | Expr "!=" Expr)
  > left Expr "&&" Expr
  > left Expr "||" Expr 
  ;

syntax Type
  = "boolean" | "integer" | "str";  
  
lexical Str = [\"]![\"]*[\"];

lexical Int 
  = [1-9][0-9]* | [0];

lexical Bool 
  = "true" | "false";


