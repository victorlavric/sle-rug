module Transform

import Syntax;
import Resolve;
import AST;
import ParseTree;
import IO;


/* 
 * Transforming QL forms
 */
 
 
/* Normalization:
 *  wrt to the semantics of QL the following
 *     q0: "" int; 
 *     if (a) { 
 *        if (b) { 
 *          q1: "" int; 
 *        } 
 *        q2: "" int; 
 *      }
 *
 *  is equivalent to
 *     if (true) q0: "" int;
 *     if (true && a && b) q1: "" int;
 *     if (true && a) q2: "" int;
 *
 * Write a transformation that performs this flattening transformation.
 *
 */

AQuestion flatten(q:question(str _, AId identifier, AType _, list[AExpr] _), AExpr e) = blockQ(e, [q], []);

list[AQuestion] flatten(blockQ(AExpr guard, list[AQuestion] ifs, list[AQuestion] elses), AExpr e) {
	return ([] | it + flatten(q, and(e, guard)) | q <- ifs) + ([] | it + flatten(q, and(e, not(guard))) | q <- elses);
}

AForm flatten(AForm f, AExpr e) {
	f.questions = ([] | it + flatten(q, e) | q <- f.questions);
	return f;
}

AForm flatten(AForm f) {
  return flatten(f, Bool(true));   
}

/* Rename refactoring:
 *
 * Write a refactoring transformation that consistently renames all occurrences of the same name.
 * Use the results of name resolution to find the equivalence class of a name.
 *
 */
 start[Form] rename(start[Form] f, loc useOrDef, str newName, UseDef useDef) {
   set[loc] toRename = {useOrDef};
   
   if (useOrDef in useDef.def)
   	 toRename += { u | <u, useOrDef> <- useDef};
   if (useOrDef in useDef.use, <useOrDef, loc definition> <- useDef)
   	 toRename += {definition} + { u | <u, definition> <- useDef};
   	 
   return visit (f) {
   	case (Question)`<Str x> <Id y> : <Type z>` => (Question)`<Str x> <Id nn> : <Type z>`
   		when y@\loc in toRename, Id nn := [Id]newName
   	case (Question)`<Str x> <Id y> : <Type z> = <Expr expression>` => (Question)`<Str x> <Id nn> : <Type z>  = <Expr expression>`
   		when y@\loc in toRename, Id nn := [Id]newName
   	case (Expr)`<Id x>` => (Expr)`<Id nn>` 
   		when x@\loc in toRename, Id nn := [Id]newName
   };
 }
 
 
 

