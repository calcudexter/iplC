%{
// This part goes directly into the generated parser

#include "scanner.hh"
#include "parser.tab.hh"

// YY_DECL is bound to the type of the scanner. The default is 
// #define YY_DECL int Scanner::yylex()
// which is generated anyway.

//If you want the type of the generated scanner to be different, do this

#undef  YY_DECL
#define YY_DECL int IPL::Scanner::yylex( IPL::Parser::semantic_type * const lval, IPL::Parser::location_type *loc )

//We want these to be the first action after a token is recognized. 

#define YY_USER_ACTION loc->step(); loc->columns(yyleng);

// token_type is a short hand for IPL::Parser::token

using token_type = IPL::Parser::token;

%}


/* %option debug */
%option nodefault
%option yyclass="Scanner"
%option noyywrap
%option c++

OR_OP                             "||"
AND_OP                            "&&"
EQ_OP                             "=="
NE_OP                             "!="
LE_OP                             "<="
GE_OP                             ">="
INC_OP                            "++"
DEC_OP                            "--"
PTR_OP                            "->"
STRUCT                            "struct"
INT                               "int"
FLOAT                             "float"
VOID                              "void"
RETURN                            "return"
IF                                "if"
ELSE                              "else"
WHILE                             "while"
FOR                               "for"
DECIMAL                           [1-9][0-9]*
BINARY                            0[bB][01]*
OCTAL                             0[0-7]*
HEX                               0[xX][0-9a-fA-F]*
EXPONENTIAL_PART                  [eE]([+-])?[0-9]+
FLOAT1                            [0-9]*\.[0-9]+
FLOAT2                            [0-9]+\.[0-9]*
IDENTIFIER                        [a-zA-Z_][a-zA-Z0-9_]*
%%

%{ /** Code executed at the beginning of yylex **/

      yylval = lval;

%}

[ \t\r\v\f]                         //skip
[\n]                                loc->lines();
\/\*(\/*\**[^\*\/]+)*(\/*\**)?\*\/    //skip
{OR_OP}                           {
       yylval->emplace< std::string >( YYText() );
       return token_type::OR_OP;
}
{AND_OP}                          {
       yylval->emplace< std::string >( YYText() );
       return token_type::AND_OP;
}
{EQ_OP}                           {
       yylval->emplace< std::string >( YYText() );
       return token_type::EQ_OP;
}
{NE_OP}                           {
       yylval->emplace< std::string >( YYText() );
       return token_type::NE_OP;
}
{LE_OP}                           {
       yylval->emplace< std::string >( YYText() );
       return token_type::LE_OP;
}
{GE_OP}                           {
       yylval->emplace< std::string >( YYText() );
       return token_type::GE_OP;
}
{INC_OP}|{DEC_OP}                 {
       yylval->emplace< std::string >( YYText() );
       return token_type::INC_OP;
}
{PTR_OP}                          {
       yylval->emplace< std::string >( YYText() );
       return token_type::PTR_OP;
}
{STRUCT}                          {
       yylval->emplace< std::string >( YYText() );
       return token_type::STRUCT;
}
{INT}                             {
       yylval->emplace< std::string >( YYText() );
       return token_type::INT;
}
{FLOAT}                           {
       yylval->emplace< std::string >( YYText() );
       return token_type::FLOAT;
}
{VOID}                            {
       yylval->emplace< std::string >( YYText() );
       return token_type::VOID;
}
{RETURN}                          {
       yylval->emplace< std::string >( YYText() );
       return token_type::RETURN;
}
{IF}                              {
       yylval->emplace< std::string >( YYText() );
       return token_type::IF;
}
{ELSE}                            {
       yylval->emplace< std::string >( YYText() );
       return token_type::ELSE;
}
{WHILE}                           {
       yylval->emplace< std::string >( YYText() );
       return token_type::WHILE;
}
{FOR}                             {
       yylval->emplace< std::string >( YYText() );
       return token_type::FOR;
}
(({FLOAT1}|{FLOAT2}){EXPONENTIAL_PART})|({FLOAT1}|{FLOAT2}) {   
       yylval->emplace< std::string >( YYText() );
       return token_type::FLOAT_CONSTANT;
}
{DECIMAL}|{BINARY}|{OCTAL}|{HEX}  {   
       yylval->emplace< std::string >( YYText() );
       return token_type::INT_CONSTANT;
}
\"([^"\\]|\\t|\\n|\\\\|(\\\"))*\" {   
       yylval->emplace< std::string >( YYText() );
       return token_type::STRING_LITERAL;  
}
{IDENTIFIER}                      {   
       yylval->emplace< std::string >( YYText() );
       return token_type::IDENTIFIER;    
}
","             return ',';
"("             return '(';
")"             return ')';
"+"             return '+';
"{"             return '{';
"}"             return '}';
";"             return ';';
"="             return '=';
"<"             return '<';
">"             return '>';
"-"             return '-';
"["             return '[';
"]"             return ']';
"*"             return '*';
"/"             return '/';
"!"             return '!';
"&"             return '&';
"."             return '.';
<<EOF>>         yyterminate();
.                                 {
       yylval->emplace< std::string >( YYText() );
       return token_type::OTHERS;
}
%%

