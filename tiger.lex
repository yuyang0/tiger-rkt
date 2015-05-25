%{
#include<stdio.h>
#include "tokens.h"
  void count();
  %}

delim           [ \t\v\n\f]
ws              {delim}+
letter          [a-zA-Z]
digit           [0-9]
id              {letter}[{letter}{digit}_]*
number          [+-]?{digit}+


%%
                        /* keywords */
while         {count(); return (WHILE);}
for           {count(); return (FOR);}
to            {count(); return (TO);}
break         {count(); return (BREAK);}
let           {count(); return (LET);}
in            {count(); return (IN);}
end           {count(); return (END);}
function      {count(); return (FUNCTION);}
var           {count(); return (VAR);}
type          {count(); return (TYPE);}
array         {count(); return (ARRAY);}
if            {count(); return (IF);}
then          {count(); return (THEN);}
 else          {count(); return (ELSE);}
do            {count(); return (DO);}
of            {count(); return (OF);}
nil           {count(); return (NIL);}


{ws}                   {count();}
"/*"                   {count(); comment();}
{id}                   {count(); return (ID);}
{number}               {count(); return (NUMBER);}
\"(\\.|[^\\\n])*\"     {count(); return (STRING);}
/* \"(\\.|[^\\"\n])*\" */
/* operators  */
"+"           {count(); return (ADD);}
"-"           {count(); return (SUB);}
"*"           {count(); return (MULT);}
"/"           {count(); return (DIV);}

"="           {count(); return (EQ);}
"<>"          {count(); return (NEQ);}
">"           {count(); return (GT);}
">="          {count(); return (GTE);}
"<"           {count(); return (LT);}
"<="          {count(); return (LTE);}
":="          {count(); return (ASSIGN);}

"&"           {count(); return (AND);}
"|"           {count(); return (OR);}
/* punctuations */
"("           {count(); return (LPAREN);}
")"           {count(); return (RPAREN);}
"["           {count(); return (LSQUARE);}
"]"           {count(); return (RSQUARE);}
"{"           {count(); return (LCURLY);}
"}"           {count(); return (RCURLY);}
","           {count(); return (COMMA);}
";"           {count(); return (SEMICOLON);}
":"           {count(); return ();}

%%
void comment(){
  char c, prev = 0;

  while ((c = input()) != 0) {      /* (EOF maps to 0) */
      if (prev == '*' && c == '/')
        return;
      if (prev == "/" && c == "*") /* nested comments */
        comment();
      prev = c;
    }
  error("unterminated comment");
}


int line = 1;
int col = 0;
void count(){
  int i = 0;
  for (i = 0; yytext[i] != '\0'; i++) {
    if (yytext[i] == '\n'){
      col = 0;
      ++line;
    }else if (yytext[i] == '\t'){
      col += 8 - (col % 8);
    }else{
      col++;
    }
  }
  printf("%s", yytext);
}

