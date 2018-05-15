module Syntax

import Prelude;

lexical Id  = ([a-z][a-z0-9]* !>> [a-z0-9]) \ PicoKeywords;
lexical Natural = [0-9]+ ;
lexical String = "\"" ![\"]*  "\"";
lexical Logical = "true"|"false";

keyword PicoKeywords = "begin" | "end" | 
                       "declare" | 
                       "if" | "then" | "else" | "fi" | 
                       "while" | "do" | "od" | "true" | "false"
                       ;

layout Layout = WhitespaceAndComment* !>> [\ \t\n\r%];

lexical WhitespaceAndComment 
   = [\ \t\n\r]
   | @category="Comment" "%" ![%]+ "%"  //multi line comment
   | @category="Comment" "%%" ![\n]* $ //one line comment
   ;

start syntax Program 
   = program: "begin" Declarations decls {(SimpleStatement|ComplexStatement)  ";"}* body "end" ;

syntax Declarations 
   = "declare" {Declaration ","}* decls ";" ;  
 
syntax Declaration = decl: Id id ":" Type tp;

syntax Type 
   = natural:"natural" 
   | string :"string"
   | boolean:"boolean"
   ;
   
/* We chose to split the original Statement symbol to Simple and Complex. The first contains just the assignment
   operation, whereas the second the rest of the statements. This was done to isolate the assignment from the other
   statements and construct a C-like for-loop which has roughly the following syntax: for assignment;expression;assignment do
*/
syntax SimpleStatement = asgStat: Id var ":="  Expression val;
syntax ComplexStatement 
   = ifElseStat: "if" Expression cond "then" {(SimpleStatement|ComplexStatement) ";"}*  thenPart "else" {(SimpleStatement|ComplexStatement) ";"}* elsePart "fi"
   | whileStat: "while" Expression cond "do" {(SimpleStatement|ComplexStatement) ";"}* body "od"
   |forStat: "for" "(" SimpleStatement ";" Expression ";" SimpleStatement ")" "do" {(SimpleStatement|ComplexStatement) ";"}* body "rof"
  ;  

/* For the precedence of the operators we followed the common precedence found in language like C or Java:
   concatenation, arithetic operators, comparisons, (in)equality, logical operators
*/
syntax Expression 
   = id: Id name
   | strCon: String string
   | natCon: Natural natcon
   | boolCon: Logical boolcon
   | bracket "(" Expression e ")"
   > left conc: Expression lhs "||" Expression rhs
   > left ( add: Expression lhs "+" Expression rhs
          | sub: Expression lhs "-" Expression rhs
          )
   > left ( gt: Expression lhs "\>" Expression rhs
   		  | lt: Expression lhs "\<" Expression rhs
   		  | ge: Expression lhs "\>=" Expression rhs
   		  | le: Expression lhs "\<=" Expression rhs
   		  )
   > left ( neq: Expression lhs "!=" Expression rhs
   		  | equ: Expression lhs "==" Expression rhs
   		  )
   > left not: "!" Expression arg
   > left and: Expression lhs "&" Expression rhs
   > left or: Expression lhs "|" Expression rhs
  ;

public start[Program] program(loc l) {
  return parse(#start[Program], l);
}

public start[Program] program(str s, loc l) {
  return parse(#start[Program], s, l);
}