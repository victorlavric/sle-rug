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
  		script(src("https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js")),
  		title("QL Form <f.name>")
  	),
  	body(
  		h1("Form <f.name>"),
  		h2("Questions : "),
  		div([ question2html(q) | q <- f.questions]),
  		script(src("tax.js"))
  		
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
			return div(div([question2html(qq) | qq <- ifs]), div([question2html(qq) | qq <- elses]));
	
		case question(str content, AId identifier, AType typeOf, list[AExpr] express): {
			HTML5Attr t = determineType(typeOf);
			if (size(express) > 0)
				return form(id("<identifier.name>-form"),label("<content>  <identifier.name>  :  <typeOf.typeOf>"), input(id("<identifier.name>-question"), t));
			else
				return form(id("<identifier.name>-form"),label("<content>  <identifier.name>  :  <typeOf.typeOf>"), input(id("<identifier.name>-question"), t));
		}
	}
}


str update(\type("boolean"), AId ident, list[AExpr] express) {
	str result = "";
	
	if (size(express) > 0) {
		result += "document.getElementById(\"<ident.name>-question\").checked = <expression2js(express[0])>;\n";
		result += "document.getElementById(\"<ident.name>-question\").readOnly = true;\n";
	} else {
		result += "document.getElementById(\"<ident.name>-question\").checked = true;\n";
	}
	
	return result;
}

str update(\type("integer"), AId ident, list[AExpr] express) {
	str result = "";
	
	if (size(express) > 0) {
		result += "document.getElementById(\"<ident.name>-question\").value = <expression2js(express[0])>;\n";
		result += "document.getElementById(\"<ident.name>-question\").readOnly = true;\n";
	} else {
		result += "document.getElementById(\"<ident.name>-question\").value = 0;\n";
	}
	
	return result;
}

str update(\type("str"), AId ident, list[AExpr] express) {
	str result = "";
	
	if (size(express) > 0) {
		result += "document.getElementById(\"<ident.name>-question\").value = <expression2js(express[0])>;\n";
		result += "document.getElementById(\"<ident.name>-question\").readOnly = true;\n";
	} else {
		result += "document.getElementById(\"<ident.name>-question\").value = \"\";\n";
	}
	
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

str form2js(AForm f) {
  str result = "";
  
  result += computeVariables(f);
  return result;
}
