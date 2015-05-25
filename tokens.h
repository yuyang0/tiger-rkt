/*Time-stamp: <2013-01-15 11:28:12 by Yu Yang>
 * =======================================================================
 *       Filename:  tokens.h
 *        Created:  2014-07-07 09:55:57
 *       Compiler:  gcc
 *
 *         Author:  Yu Yang
 *			Email:  yy1990cn@gmail.com
 * =======================================================================
 */

#ifndef _TOKENS_H_
#define _TOKENS_H_ 1
/* keywords */
#define WHILE    1
#define FOR      2
#define TO       3
#define BREAK    4
#define LET      5
#define IN       6
#define END      7
#define FUNCTION 8
#define VAR      9
#define TYPE     10
#define ARRAY    11
#define IF       12
#define THEN     13
#define ELSE     14
#define DO       15
#define OF       16
#define NIL      17

#define ID       20
#define NUMBER   30
#define STRING   40
/* arithmetical operators */
#define ADD      101
#define SUB      102
#define MULT     103
#define DIV      104
/* relational operators */
#define EQ       111
#define NEQ      112
#define GT       113
#define GTE      114
#define LT       115
#define LTE      116

#define ASSIGN   117
/* logical operators */
#define AND      121
#define OR       122

/* punctuations */
#define LPAREN      201
#define RPAREN      202
#define LSQUARE     203
#define RSQUARE     204
#define LCURLY      205
#define RCURLY      206
#define COMMA       207
#define SEMICOLON   208

#endif /* _TOKENS_H_ */

