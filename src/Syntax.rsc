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
   = program: "begin" Declarations decls {Statement  ";"}* body "end" ;

syntax Declarations 
   = "declare" {Declaration ","}* decls ";" ;  
 
syntax Declaration = decl: Id id ":" Type tp;

syntax Type 
   = natural:"natural" 
   | string :"string"
   | boolean:"boolean"
   ;

syntax Statement 
   = asgStat: Id var ":="  Expression val 
   | ifElseStat: "if" Expression cond "then" {Statement ";"}*  thenPart "else" {Statement ";"}* elsePart "fi"
   | whileStat: "while" Expression cond "do" {Statement ";"}* body "od"
 //  | forStat: "for" Id loopvar ":=" Natural "to" Natural "do" {Statement ";"}* body "rof"
   |forStat: "for" Id loopvar ":=" Natural ";" Expression ";" Expression "do" {Statement ";"}* body "rof"
  ;  
  //should we enforce the syntax in such a way that only a valid subset of exressions is used? i.e. intro
  //duce LogicalExpression. Applicable also at || operation. Should lhs and rhs be any possible expression (Natural,Logical) or only String?
     
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