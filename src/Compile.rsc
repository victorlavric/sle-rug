module Compile

import AST;
import List;
import Resolve;
import IO;
import lang::html5::DOM; // see standard library

/*
 * Implement a compiler for QL to HTML and Javascript
 *
 * - assume the form is type- and name-correct
 * - separate the compiler in two parts form2html and form2js producing 2 files
 * - use string templates to generate Javascript
 * - use the HTML5Node type and the `str toString(HTML5Node x)` function to format to string
 * - use any client web framework (e.g. Vue, React, jQuery, whatever) you like for event handling
 * - map booleans to checkboxes, strings to textfields, ints to numeric text fields v
 * - be sure to generate uneditable widgets for computed questions! v
 * - if needed, use the name analysis to link uses to definitions
 */


void compile(AForm f) {
  writeFile(f.src[extension="js"].top, form2js(f));
  writeFile(f.src[extension="html"].top, toString(form2html(f)));
}

HTML5Node form2html(AForm f) {
  return html(
  	head(
  		title("QL Form <f.name>")
  	),
  	body(
  		h1("Form <f.name>"),
  		h2("Questions : "),
  		div([ question2html(q) | q <- f.questions]),
  		script(src("<f.src[extension="js"].file>"))
  		
  	)
  );
}


HTML5Attr determineType(AType t) {
  switch (t.typeOf) {
  	case "boolean": return \type("checkbox");
  	case "integer": return \type("number");
  	case "str": return \type("text");
  }
}

HTML5Node question2html(AQuestion q) {
	switch(q) {
		case blockQ(AExpr guard, list[AQuestion] ifs, list[AQuestion] elses):
			return div(
					div(span([question2html(qq) | qq <- ifs])), 
					div(span([question2html(qq) | qq <- elses]))
				   );
	
		case question(str content, AId identifier, AType typeOf, list[AExpr] express): {
			HTML5Attr t = determineType(typeOf);
			return form(id("<identifier.name>-f"),label("<content>  <identifier.name>  :  <typeOf.typeOf>"), input(id("<identifier.name>-q"), t));
		}
	}
}


str update(\type("boolean"), AId ident, list[AExpr] express) {
	str result = "";
	
	if (size(express) > 0) 
		result += "document.getElementById(\"<ident.name>-q\").checked = <expression2js(express[0])>;\n";
	else
		result += "document.getElementById(\"<ident.name>-q\").checked = <ident.name>;\n";
	
	return result;
}

str update(\type("integer"), AId ident, list[AExpr] express) {
	str result = "";
	
	if (size(express) > 0)
		result += "document.getElementById(\"<ident.name>-q\").value = <expression2js(express[0])>;\n";
	else
		result += "document.getElementById(\"<ident.name>-q\").value = <ident.name>;\n";
	
	return result;
}

str update(\type("str"), AId ident, list[AExpr] express) {
	str result = "";
	
	if (size(express) > 0)
		result += "document.getElementById(\"<ident.name>-q\").value = <expression2js(express[0])>;\n";
	else 
		result += "document.getElementById(\"<ident.name>-q\").value = <ident.name>;\n";
	
	
	return result;
}

str computeVariables(AForm f) {
	str result = "";
	visit(f) {
		case question(str content, AId identifier, AType typeOf, list[AExpr] express): 
			result += update(typeOf, identifier, express);
	}
	return result;
}

str expression2js(AExpr e) {
	switch (e) {
		case ref(AId x): return x.name;
	    case Str(str x): return x;
	    case Int(int x): return "<x>";
	    case Bool(bool x): return "<x>";
	    case par(AExpr expr): return "(" + expression2js(expr) + ")";
	    case not(AExpr expr): return "!" + expression2js(expr);
	    case plus(AExpr lhs, AExpr rhs): return  expression2js(lhs) + "+" + expression2js(rhs); 
	    case minus(AExpr lhs, AExpr rhs): return expression2js(lhs) + "-" + expression2js(rhs);
	    case mult(AExpr lhs, AExpr rhs): return expression2js(lhs) + "*" + expression2js(rhs);
	    case div(AExpr lhs, AExpr rhs): return expression2js(lhs) + "/" + expression2js(rhs);
	    case greaterEq(AExpr lhs, AExpr rhs): return expression2js(lhs) + "\>=" + expression2js(rhs);
	    case smallerEq(AExpr lhs, AExpr rhs): return expression2js(lhs) + "\<=" + expression2js(rhs);
	    case greater(AExpr lhs, AExpr rhs): return expression2js(lhs) + "\>" + expression2js(rhs); 
	    case smaller(AExpr lhs, AExpr rhs): return expression2js(lhs) + "\<" + expression2js(rhs);
	    case equal(AExpr lhs, AExpr rhs): return expression2js(lhs) + "==" + expression2js(rhs);
	    case notEq(AExpr lhs, AExpr rhs): return expression2js(lhs) + "!=" + expression2js(rhs);
	    case and(AExpr lhs, AExpr rhs): return expression2js(lhs) + "&&" + expression2js(rhs);
	    case or(AExpr lhs, AExpr rhs): return expression2js(lhs) + "||" + expression2js(rhs);
	    default: throw "Unsupported expression <e>";
	}
}

str defaultFor(\type("boolean")) = "false";
str defaultFor(\type("integer")) = "0";
str defaultFor(\type("str")) = "\"\"";

str initVar(AForm f) {
	return (""| it + "let <identifier.name> = <defaultFor(typeOf)>;\n" | /question(str _, AId identifier, AType typeOf, list[AExpr] _) := f);
}

str precomputation(AForm f) {
	str result = "";
	visit (f) {
		case question(str content, AId identifier, AType typeOf, list[AExpr] express):
			if (size(express) > 0) {
				result += "<identifier.name> = <expression2js(express[0])>;\n";
				result += "document.getElementById(\"<identifier.name>-q\").readOnly = true;\n";
			}
	}
	return result;
}

str upToDateVars(AForm f) {
	str result = "";
	visit (f) {
		case question(_, AId identifier, AType typeOf, _):
			switch (typeOf.typeOf) {
				case "boolean":  
					result += "<identifier.name> = document.getElementById(\"<identifier.name>-q\").checked;\n"; 
				case "integer":
					result += "<identifier.name> = document.getElementById(\"<identifier.name>-q\").value;\n";
				case "str":
					result += "<identifier.name> = document.getElementById(\"<identifier.name>-q\").value;\n";
			}
			
	}
	
	return result;
}

str eventFunction(AForm f) {
  return "document.addEventListener(\"input\", function () {
			'  <upToDateVars(f)>
			'  <computeVariables(f)>
			'  <ifElseCompute(f)>
			 })";
}

str evaluate(list[AQuestion] qs) {
	str result = "";
	
	for (q <- qs) {
		if (blockQ(AExpr guard, list[AQuestion] ifs, list[AQuestion] elses) := q)
				result += ifElseDet(q);
		else
			result += "document.getElementById(\"<q.identifier.name>-f\").hidden = false;\n";
	}
	return result;
}

str ifElseDet(AQuestion q) {
  str result = "";
  
  switch(q) {
  	  case blockQ(AExpr guard, list[AQuestion] ifs, list[AQuestion] elses): {
  		  result += "if (<expression2js(guard)>) {
  		          '   <evaluate(ifs)>
  		          '   <("" | it + "document.getElementById(\"<identifier.name>-f\").hidden = true;\n" | /question(str _, AId identifier, AType _, list[AExpr] _) <- elses)>
  		          '} else {
  		          '   <("" | it + "document.getElementById(\"<identifier.name>-f\").hidden = true;\n" | /question(str _, AId identifier, AType _, list[AExpr] _) <- ifs)>
  		          '   <evaluate(elses)>
  		          '}\n";
  	  }
   }
  return result;
}

str ifElseCompute(AForm f) {
  return ("" | it + ifElseDet(q) | q <- f.questions);
}

str form2js(AForm f) {
    return initVar(f) + precomputation(f) + computeVariables(f) + ifElseCompute(f) + eventFunction(f);
}