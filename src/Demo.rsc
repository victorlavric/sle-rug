module Demo

import Syntax;
import AST;
import CST2AST;
import Check;
import Compile;
import Eval;
import Resolve;
import Transform;
import ParseTree;
import vis::ParseTree;
import IO;

start[Form] parseDemo(loc src) {
	pt = parse(#start[Form], src);
	return pt;
}

void visualParseDemo(loc src) {
	pt = parse(#start[Form], src);
	renderParsetree(pt);
}

void visualExpression(str s) {
	pt = parse(#Expr, s);
	renderParsetree(pt);
}


AForm makeASTDemo(loc src) {
	pt = parse(#start[Form], src);
	return cst2ast(pt);
}

test bool test1CST2AST() = plus(Int(2), mult(Int(3), Int(2), src = loc _), src = loc _) := cst2ast((Expr)`2 + 3*2`);

test bool test2CST2AST() = div(
								par(
									plus(Int(2), ref(id("var", src = loc _), src = loc _), 
									src = loc _), 
								src = loc _), 
		 					Int(2), src = loc _) := cst2ast((Expr)`(2 + var)/2`);
		 
test bool test3CST2AST() = smaller(Int(2), Int(3), src = loc _) := cst2ast((Expr)`2 \< 3`);

test bool test4CST2AST() = greaterEq(ref(id("myvar", src = loc _), src = loc _), Int(3), src = loc _) 
							:= cst2ast((Expr)`myvar \>= 3`);
							
test bool test5CST2AST() =  blockQ(
								  greater(
								    Int(2),
								    Int(3),
								    src=loc _),
								  [question(
								      "\"123\"",
								      id(
								        "var",
								        src=loc _),
								      \type(
								        "boolean",
								        src=loc _),
								      [],
								      src=loc _)],
								  [],
								  src=loc _)
						:= cst2ast((Question)`if (2 \> 3) { "123" var: boolean }`);			


void makeCompile(loc src) {
	pt = parse(#start[Form], src);
	ast = cst2ast(pt);
	compile(ast);
}

void interpretDemo(loc src, str variable, str v) {
	pt = parse(#start[Form], src);
	ast = cst2ast(pt);
	inp = input(variable, \vstr(v));
	println(eval(ast, inp, initialEnv(ast)));
}

void interpretDemo(loc src, str variable, int v) {
	pt = parse(#start[Form], src);
	ast = cst2ast(pt);
	inp = input(variable, \vint(v));
	println(eval(ast, inp, initialEnv(ast)));
}

void interpretDemo(loc src, str variable, bool v) {
	pt = parse(#start[Form], src);
	ast = cst2ast(pt);
	inp = input(variable, \vbool(v));
	println(eval(ast, inp, initialEnv(ast)));
}


AForm flattenDemo(loc src) {
	pt = parse(#start[Form], src);
	ast = cst2ast(pt);
	return flatten(ast);
}

test bool flattenTest1() =  form(
							  "Ex1",
							  [
							    blockQ(
							      Bool(true),
							      [question(
							          "\"123\"",
							          id(
							            "var",
							            src = loc _),
							          \type(
							            "boolean",
							            src = loc _),
							          [],
							          src = loc _)],
							      []),
							    blockQ(
							      and(
							        Bool(true),
							        ref(
							          id(
							            "var",
							            src = loc _),
							          src = loc _)),
							      [question(
							          "\"321\"",
							          id(
							            "vv",
							            src = loc _),
							          \type(
							            "boolean",
							            src = loc _),
							          [],
							          src = loc _)],
							      [])
							  ],
  								src= loc _) 
  										:= flatten(makeASTDemo(|project://QL/examples/Ex.myql|));
  										

/*

rename(parseDemo(tax), |project://QL/examples/tax.myql|(258,12,<14,6>,<14,18>), "batman", resolve(makeASTDemo(tax)).UseDef);

test testRenameUse() = rename(, loc useOrDef, str newName, UseDef useDef) 
*/