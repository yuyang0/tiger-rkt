%{
  #include "stdio.h"
  #include "tokens.h"
  %}

%token ID
%token NUMBER
%%
%%
#include "lex.yy.c"
