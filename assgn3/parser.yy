%skeleton "lalr1.cc"
%require  "3.0.1"

%defines 
%define api.namespace {IPL}
%define api.parser.class {Parser}

%define parse.trace

%code requires{
   #include "ast.hh"
   #include "location.hh"
   namespace IPL {
      class Scanner;
   }

  // # ifndef YY_NULLPTR
  // #  if defined __cplusplus && 201103L <= __cplusplus
  // #   define YY_NULLPTR nullptr
  // #  else
  // #   define YY_NULLPTR 0
  // #  endif
  // # endif

}

%printer { std::cerr << $$; } OR_OP
%printer { std::cerr << $$; } AND_OP
%printer { std::cerr << $$; } EQ_OP
%printer { std::cerr << $$; } NE_OP
%printer { std::cerr << $$; } LE_OP
%printer { std::cerr << $$; } GE_OP
%printer { std::cerr << $$; } INC_OP
%printer { std::cerr << $$; } PTR_OP
%printer { std::cerr << $$; } STRUCT
%printer { std::cerr << $$; } INT
%printer { std::cerr << $$; } FLOAT
%printer { std::cerr << $$; } VOID
%printer { std::cerr << $$; } RETURN
%printer { std::cerr << $$; } IF
%printer { std::cerr << $$; } ELSE
%printer { std::cerr << $$; } WHILE
%printer { std::cerr << $$; } FOR
%printer { std::cerr << $$; } INT_CONSTANT
%printer { std::cerr << $$; } FLOAT_CONSTANT
%printer { std::cerr << $$; } STRING_LITERAL
%printer { std::cerr << $$; } PRINTF
%printer { std::cerr << $$; } IDENTIFIER

%parse-param { Scanner  &scanner  }
%locations
%code{
   #include <iostream>
   #include <cstdlib>
   #include <fstream>
   #include <string>
   
   #include "scanner.hh"
   #include "printast.hh"

   // Map from the struct/function name to the type and the hash
   SymTab* gst = new SymTab();
   std::stack<struct SymTab*> tables;
   std::vector<std::string> curr_varlist;
   std::stack<std::string> curr_params;
   
   bool struct_func = true;

   std::string fun_rettype;
   std::string curr_func;

   std::map<std::string, abstract_astnode*> ast;

   // This is a pointer to a vector
   // that has values to be added
   void* actPtr;

   // A stack to hold the old vectors which were active;
   std::stack<void*> actVx;

   std::string curr_unit;

   std::string code = "\t.text\n";
   std::string locals = ".LC99:\n\t.string\t\"r1 : %d\\n\"\n.LC100:\n\t.string\t\"r2 : %d\\n\"\n.LC98:\n\t.string\t\"sum : %d\\n\"\n";
   int cntLocl = 0;

   int cntLabl = 2;

   std::stack<std::string> rstack;

#undef yylex
#define yylex IPL::Parser::scanner.yylex

}

%define api.value.type variant
%define parse.assert

%start program
%token <std::string> OR_OP
%token <std::string> AND_OP
%token <std::string> EQ_OP
%token <std::string> NE_OP
%token <std::string> LE_OP
%token <std::string> GE_OP
%token <std::string> INC_OP
%token <std::string> PTR_OP
%token <std::string> STRUCT
%token <std::string> INT
%token <std::string> FLOAT
%token <std::string> VOID
%token <std::string> RETURN
%token <std::string> IF
%token <std::string> ELSE
%token <std::string> WHILE
%token <std::string> FOR
%token <std::string> PRINTF
%token <std::string> IDENTIFIER
%token <std::string> INT_CONSTANT
%token <std::string> FLOAT_CONSTANT
%token <std::string> STRING_LITERAL
%token <std::string> OTHERS
%token ',' '(' ')' '+' '{' '}' ';' '=' '<' '>' '-' '[' ']' '*' '/' '!' '&' '.'

%nterm <struct attrib> program
                       translation_unit
                       struct_specifier
                       function_definition
                       declaration_list
                       fun_declarator
                       compound_statement
                       parameter_list
                       parameter_declaration
                       declarator
                       declarator_arr
                       statement
                       statement_list
                       selection_statement
                       iteration_statement
                       assignment_statement
                       procedure_call
                       expression
                       unary_expression
                       assignment_expression
                       expression_list
                       logical_and_expression
                       equality_expression
                       relational_expression
                       additive_expression
                       multiplicative_expression
                       postfix_expression
                       primary_expression
                       declaration
                       declarator_list
                       unary_operator
                       type_specifier
                       printf_call

%%

program:
         {
            initRstack();
         }
         translation_unit
         {}
         ;

translation_unit:
         translation_unit struct_specifier
         | translation_unit function_definition
         | struct_specifier
         | function_definition
         ;

struct_specifier:
         STRUCT IDENTIFIER '{'
         {
            if(gst->tab.find($1 + " " + $2) == gst->tab.end()) {
               stRecord* rec = new stRecord();
               rec->symname = $1 + " " + $2;
               rec->var_func = "struct";
               rec->param_local = "global";
               rec->type = "-";
               
               // Using a dummy size now
               rec->size = 0;
               rec->offset = 0;

               rec->symtab = new SymTab();
               rec->symtab->curr_offset = 0;
               
               gst->tab[rec->symname] = rec;

               // declaration_list now has access to this lst
               tables.push(rec->symtab);

               struct_func = true;
               curr_unit = $1 + " " + $2;
            }
            else {
               // Error : Already present

               error(@1, "\"" + $1 + " " + $2 + "\" has a previous definition");
            }
         }
         declaration_list '}' ';'
         {
            gst->tab[$1 + " " + $2]->size = $5.size;
            
            // Need to pop the symtab now
            tables.pop();

            curr_unit = "";
         }
         ;

function_definition:
         type_specifier fun_declarator
         {
            // fun_declarator has pushed the lst to the stack
            // so compound_statement has access to the lst

            struct_func = false;

            fun_rettype = $1.text;
            curr_func = $2.text;
            gst->tab[$2.text]->type = $1.text;

            gen("\t.globl\t" + $2.text);
            gen("\t.type\t" + $2.text + ", @function");
            gen($2.text + ":");
            gen("\tpushl\t%ebp");
            gen("\tmovl\t%esp, %ebp");
         }
         compound_statement
         {
            // Change the return type of the function here

            // gst->tab[$2.text]->type = $1.text;

            ast[$2.text] = $4.ast;

            if(!$4.ret) {
               if($2.text != "main") {
                  gen("\tnop");
               }
               else {
                  // gen("\tmovl\t$0, %eax");
               }
            }
            if(!$4.non_ret) {
               // Note that popl is used when there's no inner statement
               // gen("\tpopl\t%ebp");
               gen("\tleave");
            }
            else {
               // Else leave is used
               gen("\tleave");
            }
            gen("\tret");
            gen("\t.size\t" + $2.text + ", .-" + $2.text);
            tables.pop();
         }
         ;

type_specifier:
         VOID
         {
            $$.text = "void";
         }
         | INT
         {
            $$.text = "int";
            $$.size = 1;
         }
         | STRUCT IDENTIFIER
         {
            $$.text = $1 + " " + $2;
            if(gst->tab.find($$.text) == gst->tab.end()) {
               // Error : Not defined

               error(@1, "\"" + $$.text + "\" is not defined");
            }
            $$.size = gst->tab[$$.text]->size/4;
         }
         ;

fun_declarator:
         IDENTIFIER '('
         {
            if(gst->tab.find($1) == gst->tab.end()) {
               stRecord* rec = new stRecord();
               rec->symname = $1;
               rec->var_func = "fun";
               rec->param_local = "global";
               rec->type = fun_rettype;
               
               // Using a dummy size now
               rec->size = 0;
               rec->offset = 0;

               rec->symtab = new SymTab();
               rec->symtab->curr_offset = 12;
               
               gst->tab[rec->symname] = rec;

               // parameter_list now has access to lst
               tables.push(rec->symtab);
            }
            else {
               // Error : Already present

               error(@1, "The function \"" + $1 + "\" has a previous definition");
            }
         }
         parameter_list ')'
         {
            $$.text = $1;

            // Here, I have the stack of parameters completely
            // Now, I need to set the offsets correctly

            SymTab* lst = tables.top();
            
            while(!curr_params.empty()) {
               std::string param = curr_params.top();
               curr_params.pop();

               lst->tab[param]->offset = lst->curr_offset;
               // Here, the curr_offset will be accumulated with
               // size only when it's not an array
               if(isArray(lst->tab[param]->type)) {
                  lst->curr_offset += 4;
               }
               else {
                  lst->curr_offset += lst->tab[param]->size;
               }
            }

            lst->curr_offset = 0;
         }
         | IDENTIFIER '(' ')'
         {
            if(gst->tab.find($1) == gst->tab.end()) {
               stRecord* rec = new stRecord();
               rec->symname = $1;
               rec->var_func = "fun";
               rec->param_local = "global";
               rec->type = "-";
               
               // Using a dummy size now
               rec->size = 0;
               rec->offset = 0;

               rec->symtab = new SymTab();
               rec->symtab->curr_offset = 0;
               
               gst->tab[rec->symname] = rec;
               tables.push(rec->symtab);

               $$.text = $1;
            }
            else {
               // Error : Already present

               error(@1, "The function \"" + $1 + "\" has a previous definition");
            }
         }
         ;

parameter_list:
         parameter_declaration
         {}
         | parameter_list ',' parameter_declaration
         {}
         ;

parameter_declaration:
         type_specifier declarator
         {
            // Set only the base type of the declarator here

            SymTab* lst = tables.top();

            if(lst->tab[$2.text]->type[0] != '*') {
               if($1.text == "void") {
                  error(@1, "Cannot declare the type of a parameter as \"void\"");
               }

               lst->tab[$2.text]->size = lst->tab[$2.text]->size*$1.size;
            }
            lst->tab[$2.text]->type = $1.text + lst->tab[$2.text]->type;
            lst->tab[$2.text]->param_local = "param";

            curr_params.push($2.text);

            //
         }
         ;

declarator_arr:
         IDENTIFIER
         {
            // Add the identifier to the table here

            stRecord* rec = new stRecord();
            rec->symname = $1;
            rec->var_func = "var";
            rec->type = "";

            // Dummy values for size and offset
            rec->size = 0;
            rec->offset = 0;

            rec->symtab = nullptr;

            // Added the identifier to the lst
            SymTab* lst = tables.top();
            
            if(lst->tab.find($1) == lst->tab.end()) {
               lst->tab[$1] = rec;

               $$.text = $1;
               $$.size = 4;
            }
            else {
               // Error : Already present

               delete rec;
               error(@1, "\"" + $1 + "\" has a previous declaration");
            }
         }
         | declarator_arr '[' INT_CONSTANT ']'
         {
            // Have added the identifier to the lst
            // Need to modify the type and other things

            // Still the same lst will be at the top
            SymTab* lst = tables.top();
            lst->tab[$1.text]->type = lst->tab[$1.text]->type + "[" + $3 + "]";

            $$.text = $1.text;
            $$.size = $1.size*stoi($3);

            if(stoi($3) < 0) {
               // Error : Size of array is negative

               error(@1, "Size of array is negative");
            }
         }
         ;

M:
         %empty
         {
            actVx.push(actPtr);
            actPtr = new std::vector<exp_astnode*>();
         }
         ;


declarator:
         declarator_arr
         {
            // Just carry the variable name above

            $$.text = $1.text;
            $$.size = $1.size;

            SymTab* lst = tables.top();
            lst->tab[$1.text]->size = $1.size;
         }
         | '*' declarator
         {
            // Have the variable name and lst as well

            SymTab* lst = tables.top();
            lst->tab[$2.text]->type = "*" + lst->tab[$2.text]->type;
            lst->tab[$2.text]->size = $2.size;

            $$.text = $2.text;
            $$.size = $2.size;
         }
         ;

compound_statement:
         '{' '}'
         {
            seq_astnode* obj = new seq_astnode();
            $$.ast = obj;
            $$.non_ret = false;
            $$.ret = false;
            $$.size = 0;
         }
         | '{'
         {
            actPtr = new std::vector<statement_astnode*>();
         }
         statement_list '}'
         {
            seq_astnode* obj = new seq_astnode();
            obj->stmts = *(std::vector<statement_astnode*> *) actPtr;
            $$.ast = obj;

            if(actVx.empty())
               actPtr = nullptr;
            else {
               actPtr = actVx.top();
               actVx.pop();
            }

            $$.non_ret = $3.non_ret;
            $$.ret = $3.ret;
            $$.size = 0;

            genCode($$.ast);
         }
         | '{' declaration_list '}'
         {
            seq_astnode* obj = new seq_astnode();
            $$.ast = obj;

            $$.non_ret = false;
            $$.ret = false;
            $$.size = $2.size;
         }
         | '{' declaration_list
         {
            actPtr = new std::vector<statement_astnode*>();
            
            // Here, I need to check if the size of declaration list
            // is non-zero and make space on the stack as necessary

            if($2.size != 0) {
               gen("\tsubl\t$" + std::to_string($2.size) + ", %esp");
            }
         }
         statement_list '}'
         {
            // Have the lst at the top, use it
            // Modified the declaration_list appropriately

            seq_astnode* obj = new seq_astnode();
            obj->stmts = *(std::vector<statement_astnode*> *) actPtr;
            $$.ast = obj;
            $$.size = $2.size;

            if(actVx.empty())
               actPtr = nullptr;
            else {
               actPtr = actVx.top();
               actVx.pop();
            }

            $$.non_ret = $4.non_ret;
            $$.ret = $4.ret;

            genCode($$.ast);
         }
         ;

statement_list:
         statement
         {
            ((std::vector<statement_astnode*> *) actPtr)->push_back((statement_astnode *) $1.ast);
            $$.non_ret = $1.non_ret;
            $$.ret = $1.ret;
         }
         | statement_list statement
         {
            ((std::vector<statement_astnode*> *) actPtr)->push_back((statement_astnode *) $2.ast);
            $$.non_ret = $1.non_ret || $2.non_ret;
            $$.ret = $1.ret || $2.ret;
         }
         ;

statement: 
         ';'
         {
            $$.ast = new empty_astnode();
            $$.non_ret = false;
            $$.ret = false;
         }
         | '{'
         {
            actVx.push(actPtr);
            actPtr = new std::vector<statement_astnode*>();
         }
         statement_list '}'
         {
            seq_astnode* obj = new seq_astnode();
            obj->stmts = *(std::vector<statement_astnode*> *) actPtr;
            $$.ast = obj;

            if(actVx.empty())
               actPtr = nullptr;
            else {
               actPtr = actVx.top();
               actVx.pop();
            }

            $$.non_ret = $3.non_ret;
            $$.ret = $3.ret;
         }
         | selection_statement
         {
            $$.ast = $1.ast;
            $$.non_ret = true;
            $$.ret = $1.ret;
         }
         | iteration_statement
         {
            $$.ast = $1.ast;
            $$.non_ret = true;
            $$.ret = $1.ret;
         }
         | assignment_statement
         {
            $$.ast = $1.ast;
            $$.non_ret = true;
            $$.ret = false;
         }
         | procedure_call
         {
            $$.ast = $1.ast;
            $$.non_ret = true;
            $$.ret = false;
         }
         | printf_call
         {
            $$.ast = $1.ast;
            $$.non_ret = true;
            $$.ret = false;
         }
         | RETURN expression ';'
         {
            return_astnode* obj = new return_astnode();
            obj->retExp = (exp_astnode *) $2.ast;
            obj->ret_type = new std::string(fun_rettype);
            obj->fun_name = new std::string(curr_func);
            $$.ast = obj;

            std::string ret = fun_rettype;

            std::string expType = $2.type;

            if(ret == expType) {

            }
            else {
               // Error : Incompatible type returned

               error(@1, "Incompatible type \"" + expType + "\" returned, expected \"" + ret + "\"");
            }
            $$.non_ret = false;
            $$.ret = true;

            // Generate the code for return value here
            // gen("\tmovl\t" + $2.text + ", %eax");
         }
         ;

assignment_expression:
         unary_expression '=' expression
         {
            assignE_astnode* obj = new assignE_astnode();
            obj->lExp = (exp_astnode *) $1.ast;
            obj->rExp = (exp_astnode *) $3.ast;
            $$.ast = obj;
            $$.type = $1.type;

            std::string t1 = $1.type, t2 = $3.type;

            if(!$1.lval) {
               error(@1, "Left operand of assignment should have an lvalue");
            }

            if(baseType(t1) == "void") {
               if(isArray(t1)) {
                  // Invalid

                  error(@1, "Incompatible assignment when assigning to type \"" + t1 + "\" from type \"" + t2 + "\"");
               }
               else if($3.text == "0" || $3.type == "void*") {
                  // Valid
               }
               else if(depth(t1) == 1) {
                  if(depth(t2) > 0) {
                     // Valid
                  }
                  else {
                     // Invalid

                     error(@1, "Incompatible assignment when assigning to type \"" + t1 + "\" from type \"" + t2 + "\"");
                  }
               }
               else {
                  if(baseType(t2) == "void" && depth(t1) == depth(t2)) {
                     // Valid

                     // void **a;
                     // void *b[2][2][2];
                     // a = b;

                     // The above doesn't work in my code but works in their
                  }
                  else {
                     error(@1, "Incompatible assignment when assigning to type \"" + t1 + "\" from type \"" + t2 + "\"");
                  }
               }

               $$.type = "int";
            }
            else if(depth(t1) > 0) {
               if(isArray(t1)) {
                  // Invalid

                  error(@1, "Incompatible assignment when assigning to type \"" + t1 + "\" from type \"" + t2 + "\"");
               }
               else {
                  if($3.text == "0" || $3.type == "void*") {
                     // Valid
                  }
                  else if(depth(t1) != depth(t2)) {
                     // Invalid

                     error(@1, "Incompatible assignment when assigning to type \"" + t1 + "\" from type \"" + t2 + "\"");
                  }
                  else {
                     if(baseType(t1) != baseType(t2)) {
                        // Invalid
                        error(@1, "Incompatible assignment when assigning to type \"" + t1 + "\" from type \"" + t2 + "\"");
                     }
                     else {
                        if(!isArray(t2)) {
                           if(std::count(t2.begin(), t2.end(), '[') == 0) {
                              // Valid
                           }
                           else {
                              // Invalid
                              error(@1, "Incompatible assignment when assigning to type \"" + t1 + "\" from type \"" + t2 + "\"");
                           }
                        }
                        else if(std::count(t2.begin(), t2.end(), '[') == 1) {
                           // Valid
                        }
                        else {
                           // Invalid
                           error(@1, "Incompatible assignment when assigning to type \"" + t1 + "\" from type \"" + t2 + "\"");
                        }
                     }
                  }
               }
            }
            else {
               // Base type is not void and non-pointer LHS

               if(t1 == "int") {
                  if(t2 == "int") {
                     // Valid
                     $$.type = t1;
                  }
                  else {
                     error(@1, "Incompatible assignment when assigning to type \"" + t1 + "\" from type \"" + t2 + "\"");
                  }
               }
               else if(t1 == t2) {
                  // Valid
               }
               else {
                  error(@1, "Incompatible assignment when assigning to type \"" + t1 + "\" from type \"" + t2 + "\"");
               }
            }

            $$.text = $1.text + "=" + $3.text;
            obj->type = $$.type;
            $$.lval = false;
         }
         ;

assignment_statement:
         assignment_expression ';'
         {
            assignS_astnode* obj = new assignS_astnode();
            obj->lExp = ((assignE_astnode *) $1.ast)->lExp;
            obj->rExp = ((assignE_astnode *) $1.ast)->rExp;
            $$.ast = obj;

            // Code generated already
         }
         ;

printf_call:
         PRINTF '(' STRING_LITERAL ')' ';'
         {
            proccall_astnode* obj = new proccall_astnode();
            identifier_astnode *id = new identifier_astnode();
            id->name = new std::string($1);
            obj->fname = id;
            string_astnode *str = new string_astnode();
            str->str = new std::string($3);
            obj->callExps.push_back((exp_astnode *) str);

            $$.ast = obj;
         }
         | PRINTF '(' STRING_LITERAL ',' M 
         {
            string_astnode *str = new string_astnode();
            str->str = new std::string($3);
            (*(std::vector<exp_astnode*> *) actPtr).push_back((exp_astnode *) str);
         }
         expression_list ')' ';'
         {
            proccall_astnode* obj = new proccall_astnode();
            identifier_astnode *id = new identifier_astnode();
            id->name = new std::string($1);
            obj->fname = id;
            obj->callExps = *(std::vector<exp_astnode*> *) actPtr;
            $$.ast = obj;

            actPtr = actVx.top();
            actVx.pop();
         }
         ;

procedure_call:
         IDENTIFIER '(' ')' ';'
         {
            if(($1 != "printf" && $1 != "scanf") && gst->tab.find($1) == gst->tab.end()) {
               // Error : Not declared

               error(@1, "Procedure \"" + $1 + "\" not declared");
            }
            else {
               proccall_astnode* obj = new proccall_astnode();
               identifier_astnode *id = new identifier_astnode();
               id->name = new std::string($1);
               id->st = gst;
               obj->fname = id;
               $$.ast = obj;

               if($1 != "printf" && $1 != "scanf") {
                  int args = 0;
                  std::map<std::string, stRecord*> tab = gst->tab[$1]->symtab->tab;

                  for(auto it = tab.begin(); it != tab.end(); ++it) {
                     if(it->second->param_local == "param") {
                        args++;
                     }
                  }

                  if(args > 0) {
                     // Error : Too few arguments

                     error(@1, "Procedure \"" + $1 + "\" called with too few arguments");
                  }
               }
            }
         }
         | IDENTIFIER '('
         M
         expression_list ')' ';'
         {
            if(($1 != "printf" && $1 != "scanf") && gst->tab.find($1) == gst->tab.end()) {
               // Error : Not declared

               error(@1, "Procedure \"" + $1 + "\" not declared");
            }
            proccall_astnode* obj = new proccall_astnode();
            identifier_astnode *id = new identifier_astnode();
            id->name = new std::string($1);
            id->st = gst;
            obj->fname = id;
            obj->callExps = *(std::vector<exp_astnode*> *) actPtr;
            $$.ast = obj;

            actPtr = actVx.top();
            actVx.pop();

            if($1 != "printf" && $1 != "scanf") {
               int args = 0;
               std::map<std::string, stRecord*> tab = gst->tab[$1]->symtab->tab;

               for(auto it = tab.begin(); it != tab.end(); ++it) {
                  if(it->second->param_local == "param") {
                     args++;
                  }
               }

               if(args > (int) obj->callExps.size()) {
                  // Error : Too few arguments

                  error(@1, "Procedure \"" + $1 + "\" called with too few arguments");
               }
               else if(args < (int) obj->callExps.size()) {
                  // Error : Too many arguments

                  error(@1, "Procedure \"" + $1 + "\" called with too many arguments");
               }
            }

            if($1 != "printf" && $1 != "scanf") {
               // See the lst of this function
               // Have a look at the parameters and sort by offsets
               // Store their types and then type cast the expressions

               std::vector<std::pair<int, std::string>> params;
               struct SymTab st = *(gst->tab[$1]->symtab);

               for(auto it = st.tab.begin(); it != st.tab.end(); ++it) {
                  stRecord entry = *(it->second);
                  if(entry.offset > 0)
                     params.push_back({entry.offset, entry.type});
               }

               sort(params.begin(), params.end(), std::greater<std::pair<int, std::string>>());

               for(int i = 0; i < (int) params.size(); i++) {
                  // exp_astnode* exp = obj->callExps[i];
                  std::string t1 = params[i].second, t2 = $4.type_list[i];

                  if(baseType(t1) == "void") {
                     if(depth(t1) == 1) {
                        if(depth(t2) > 0) {
                           // Valid
                        }
                        else {
                           // Invalid

                           error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                        }
                     }
                     else {
                        if(baseType(t2) == "void" && depth(t1) == depth(t2)) {
                           // Valid
                           std::cout << t1 << " and " << t2 << std::endl;
                        }
                        else {
                           error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                        }
                     }
                  }
                  else if(depth(t1) > 0) {
                     if(isArray(t1)) {
                        if(depth(t1) == 1) {
                           if(depth(t2) == 1) {
                              // Valid
                           }
                           else {
                              // Invalid

                              error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                           }
                        }
                        else {
                           int open_ind = t1.find('[');
                           int close_ind = t1.find(']', open_ind);
                           
                           std::string type = t1.substr(0, open_ind);
                           type += "(*)";
                           type += t1.substr(close_ind + 1);

                           if(type[type.length() - 1] == ')') {
                              type = t1.substr(0, open_ind) + "*";
                           }

                           if(depth(t2) != depth(t1)) {
                              // Invalid

                              error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                           }
                           else {
                              // Valid

                              if(isArray(t2)) {
                                 int open_ind = t2.find('[');
                                 int close_ind = t2.find(']', open_ind);
                                 
                                 std::string t2type = t2.substr(0, open_ind);
                                 t2type += "(*)";
                                 t2type += t2.substr(close_ind + 1);

                                 if(t2type[t2type.length() - 1] == ')') {
                                    t2type = t2.substr(0, open_ind) + "*";
                                 }

                                 if(t2type == type) {
                                    // Valid
                                 }
                                 else {
                                    // Invalid
                                    
                                    error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                                 }
                              }
                              else {
                                 if(t2 == type) {
                                    // Valid
                                 }
                                 else {
                                    // Invalid
                                    
                                    error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                                 }
                              }

                              // error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                           }
                        }
                     }
                     else {
                        // Non arrays
                        // Can only be simple pointers of int, float or struct

                        if(baseType(t2) == "void") {
                           if(depth(t2) == 1) {
                              // Valid
                           }
                           else {
                              // Invalid

                              error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                           }
                        }
                        else {
                           if(isArray(t2)) {
                              // Match the converted type

                              int open_ind = t2.find('[');
                              int close_ind = t2.find(']', open_ind);
                              
                              std::string t2type = t2.substr(0, open_ind);
                              t2type += "(*)";
                              t2type += t2.substr(close_ind + 1);

                              if(t2type[t2type.length() - 1] == ')') {
                                 t2type = t2.substr(0, open_ind) + "*";
                              }

                              if(t2type == t1) {
                                 // Valid
                              }
                              else {
                                 // Invalid
                                 
                                 error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                              }
                           }
                           else {
                              if(std::count(t2.begin(), t2.end(), ')') > 0) {
                                 // Invalid

                                 error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                              }
                              else if(t1 == t2) {
                                 // Valid
                              }
                              else {
                                 // Invalid

                                 error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                              }
                           }
                        }
                     }
                  }
                  else {
                     // Base type in LHS
                     if(depth(t2) != 0) {
                        // Invalid

                        error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                     }
                     else if(t1.substr(0, 6) == "struct") {
                        if(t1 == t2) {
                           // Valid
                        }
                        else {
                           // Invalid

                           error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                        }
                     }
                     else if(t1 == "int") {
                        if(t2 != t1) {
                           // Invalid

                           error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                        }
                     }
                     else {
                        // Never occurs
                     }
                  }
               }
            }
         }
         ;

expression:
         logical_and_expression
         {
            $$.ast = $1.ast;
            $$.type = $1.type;
            $$.text = $1.text;
            $$.lval = $1.lval;
         }
         | expression OR_OP logical_and_expression
         {
            op_binary_astnode* obj = new op_binary_astnode();
            obj->lExp = (exp_astnode *) $1.ast;
            obj->rExp = (exp_astnode *) $3.ast;
            obj->op = new std::string("OR_OP");
            $$.ast = obj;

            bool c1 = isInt($1.type);
            bool c2 = isInt($3.type);

            if(!(c1 && c2)) {
               error(@1, "Invalid operand of ||, not scalar or pointer");
            }

            $$.type = "int";
            obj->type = $$.type;
            $$.text = $1.text + "||" + $3.text;
            $$.lval = false;
         }
         ;

logical_and_expression:
         equality_expression
         {
            $$.ast = $1.ast;
            $$.type = $1.type;
            $$.text = $1.text;
            $$.lval = $1.lval;
         }
         | logical_and_expression AND_OP equality_expression
         {
            op_binary_astnode* obj = new op_binary_astnode();
            obj->lExp = (exp_astnode *) $1.ast;
            obj->rExp = (exp_astnode *) $3.ast;
            obj->op = new std::string("AND_OP");
            $$.ast = obj;

            bool c1 = isInt($1.type);
            bool c2 = isInt($3.type);

            if(!(c1 && c2)) {
               error(@1, "Invalid operand of ||, not scalar or pointer");
            }

            // Dummy type
            $$.type = "int";
            obj->type = $$.type;
            $$.text = $1.text + "&&" + $3.text;
            $$.lval = false;
         }
         ;

equality_expression:
         relational_expression
         {
            $$.ast = $1.ast;
            $$.type = $1.type;
            $$.text = $1.text;
            $$.lval = $1.lval;
         }
         | equality_expression EQ_OP relational_expression
         {
            op_binary_astnode* obj = new op_binary_astnode();
            obj->lExp = (exp_astnode *) $1.ast;
            obj->rExp = (exp_astnode *) $3.ast;
            obj->op = new std::string("EQ_OP");
            $$.ast = obj;

            std::string t1 = $1.type, t2 = $3.type;
            if((t1 == "int") && (t2 == "int")) {
               // Valid
            }
            else if(baseType(t1) == baseType(t2)) {
               if(baseType(t1).substr(0, 6) == "struct" && depth(t1) == 0) {
                  error(@1, "Invalid operands types for binary ==, \"" + t1 + "\" and \"" + t2 + "\"");
               }
               else if(depth(t1) != depth(t2)) {
                  error(@1, "Invalid operands types for binary ==, \"" + t1 + "\" and \"" + t2 + "\"");
               }
               else {
                  if(isArray(t1) && isArray(t2)) {
                     if(modType(t1) == modType(t2)) {
                        // Valid
                     }
                     else {
                        error(@1, "Invalid operands types for binary ==, \"" + t1 + "\" and \"" + t2 + "\"");
                     }
                  }
                  else if(isPointer(t1) && isPointer(t2)) {
                     // Valid because same depth
                  }
                  else {
                     if(modType(t1) == modType(t2)) {
                        // Valid
                     }
                     else {
                        error(@1, "Invalid operands types for binary ==, \"" + t1 + "\" and \"" + t2 + "\"");
                     }
                  }
               }
            }
            else {
               error(@1, "Invalid operands types for binary ==, \"" + t1 + "\" and \"" + t2 + "\"");
            }

            // Valid types

            if(isInt(t1)&& isInt(t2)) {
               // *(obj->op) += "_INT";

               $$.type = "int";
            }
            else {
               // Error
            }

            obj->type = $$.type;
            $$.text = $1.text + "==" + $3.text;
            $$.lval = false;
         }
         | equality_expression NE_OP relational_expression
         {
            op_binary_astnode* obj = new op_binary_astnode();
            obj->lExp = (exp_astnode *) $1.ast;
            obj->rExp = (exp_astnode *) $3.ast;
            obj->op = new std::string("NE_OP");
            $$.ast = obj;

            std::string t1 = $1.type, t2 = $3.type;
            /*if((t1 == "int") && (t2 == "int")) {
               // Valid
            }
            else if(baseType(t1) == baseType(t2)) {
               if(baseType(t1).substr(0, 6) == "struct" && depth(t1) == 0) {
                  error(@1, "Invalid operands types for binary !=, \"" + t1 + "\" and \"" + t2 + "\"");
               }
               else if(depth(t1) != depth(t2)) {
                  error(@1, "Invalid operands types for binary !=, \"" + t1 + "\" and \"" + t2 + "\"");
               }
               else {
                  if(isArray(t1) && isArray(t2)) {
                     if(modType(t1) == modType(t2)) {
                        // Valid
                     }
                     else {
                        error(@1, "Invalid operands types for binary !=, \"" + t1 + "\" and \"" + t2 + "\"");
                     }
                  }
                  else if(isPointer(t1) && isPointer(t2)) {
                     // Valid because same depth
                  }
                  else {
                     if(modType(t1) == modType(t2)) {
                        // Valid
                     }
                     else {
                        error(@1, "Invalid operands types for binary !=, \"" + t1 + "\" and \"" + t2 + "\"");
                     }
                  }
               }
            }
            else {
               error(@1, "Invalid operands types for binary !=, \"" + t1 + "\" and \"" + t2 + "\"");
            }*/

            if(isInt(t1)&& isInt(t2)) {
               // *(obj->op) += "_INT";

               $$.type = "int";
            }
            else {
               // Error
            }

            obj->type = $$.type;
            $$.text = $1.text + "!=" + $3.text;
            $$.lval = false;
         }
         ;

relational_expression:
         additive_expression
         {
            $$.ast = $1.ast;
            $$.type = $1.type;
            $$.text = $1.text;
            $$.lval = $1.lval;
         }
         | relational_expression '<' additive_expression
         {
            op_binary_astnode* obj = new op_binary_astnode();
            obj->lExp = (exp_astnode *) $1.ast;
            obj->rExp = (exp_astnode *) $3.ast;
            obj->op = new std::string("LT_OP");
            $$.ast = obj;

            std::string t1 = $1.type, t2 = $3.type;
            if((t1 == "int") && (t2 == "int")) {
               // Valid
            }
            else if(baseType(t1) == baseType(t2)) {
               if(baseType(t1).substr(0, 6) == "struct" && depth(t1) == 0) {
                  error(@1, "Invalid operands types for binary <, \"" + t1 + "\" and \"" + t2 + "\"");
               }
               else if(depth(t1) != depth(t2)) {
                  error(@1, "Invalid operands types for binary <, \"" + t1 + "\" and \"" + t2 + "\"");
               }
               else {
                  if(isArray(t1) && isArray(t2)) {
                     if(modType(t1) == modType(t2)) {
                        // Valid
                     }
                     else {
                        error(@1, "Invalid operands types for binary <, \"" + t1 + "\" and \"" + t2 + "\"");
                     }
                  }
                  else if(isPointer(t1) && isPointer(t2)) {
                     // Valid because same depth
                  }
                  else {
                     if(modType(t1) == modType(t2)) {
                        // Valid
                     }
                     else {
                        error(@1, "Invalid operands types for binary <, \"" + t1 + "\" and \"" + t2 + "\"");
                     }
                  }
               }
            }
            else {
               error(@1, "Invalid operands types for binary <, \"" + t1 + "\" and \"" + t2 + "\"");
            }

            if(isInt(t1)&& isInt(t2)) {
               // *(obj->op) += "_INT";

               $$.type = "int";
            }
            else {
               // Error
            }

            obj->type = $$.type;
            $$.text = $1.text + "<" + $3.text;
            $$.lval = false;
         }
         | relational_expression '>' additive_expression
         {
            op_binary_astnode* obj = new op_binary_astnode();
            obj->lExp = (exp_astnode *) $1.ast;
            obj->rExp = (exp_astnode *) $3.ast;
            obj->op = new std::string("GT_OP");
            $$.ast = obj;

            std::string t1 = $1.type, t2 = $3.type;
            if((t1 == "int") && (t2 == "int")) {
               // Valid
            }
            else if(baseType(t1) == baseType(t2)) {
               if(baseType(t1).substr(0, 6) == "struct" && depth(t1) == 0) {
                  error(@1, "Invalid operands types for binary >, \"" + t1 + "\" and \"" + t2 + "\"");
               }
               else if(depth(t1) != depth(t2)) {
                  error(@1, "Invalid operands types for binary >, \"" + t1 + "\" and \"" + t2 + "\"");
               }
               else {
                  if(isArray(t1) && isArray(t2)) {
                     if(modType(t1) == modType(t2)) {
                        // Valid
                     }
                     else {
                        error(@1, "Invalid operands types for binary >, \"" + t1 + "\" and \"" + t2 + "\"");
                     }
                  }
                  else if(isPointer(t1) && isPointer(t2)) {
                     // Valid because same depth
                  }
                  else {
                     if(modType(t1) == modType(t2)) {
                        // Valid
                     }
                     else {
                        error(@1, "Invalid operands types for binary >, \"" + t1 + "\" and \"" + t2 + "\"");
                     }
                  }
               }
            }
            else {
               error(@1, "Invalid operands types for binary >, \"" + t1 + "\" and \"" + t2 + "\"");
            }

            if(isInt(t1)&& isInt(t2)) {
               // *(obj->op) += "_INT";

               $$.type = "int";
            }
            else {
               // Error
            }

            obj->type = $$.type;
            $$.text = $1.text + ">" + $3.text;
            $$.lval = false;
         }
         | relational_expression LE_OP additive_expression
         {
            op_binary_astnode* obj = new op_binary_astnode();
            obj->lExp = (exp_astnode *) $1.ast;
            obj->rExp = (exp_astnode *) $3.ast;
            obj->op = new std::string("LE_OP");
            $$.ast = obj;

            std::string t1 = $1.type, t2 = $3.type;
            if((t1 == "int") && (t2 == "int")) {
               // Valid
            }
            else if(baseType(t1) == baseType(t2)) {
               if(baseType(t1).substr(0, 6) == "struct" && depth(t1) == 0) {
                  error(@1, "Invalid operands types for binary <=, \"" + t1 + "\" and \"" + t2 + "\"");
               }
               else if(depth(t1) != depth(t2)) {
                  error(@1, "Invalid operands types for binary <=, \"" + t1 + "\" and \"" + t2 + "\"");
               }
               else {
                  if(isArray(t1) && isArray(t2)) {
                     if(modType(t1) == modType(t2)) {
                        // Valid
                     }
                     else {
                        error(@1, "Invalid operands types for binary <=, \"" + t1 + "\" and \"" + t2 + "\"");
                     }
                  }
                  else if(isPointer(t1) && isPointer(t2)) {
                     // Valid because same depth
                  }
                  else {
                     if(modType(t1) == modType(t2)) {
                        // Valid
                     }
                     else {
                        error(@1, "Invalid operands types for binary <=, \"" + t1 + "\" and \"" + t2 + "\"");
                     }
                  }
               }
            }
            else {
               error(@1, "Invalid operands types for binary <=, \"" + t1 + "\" and \"" + t2 + "\"");
            }

            if(isInt(t1)&& isInt(t2)) {
               // *(obj->op) += "_INT";

               $$.type = "int";
            }
            else {
               // Error
            }

            obj->type = $$.type;
            $$.text = $1.text + "<=" + $3.text;
            $$.lval = false;
         }
         | relational_expression GE_OP additive_expression
         {
            op_binary_astnode* obj = new op_binary_astnode();
            obj->lExp = (exp_astnode *) $1.ast;
            obj->rExp = (exp_astnode *) $3.ast;
            obj->op = new std::string("GE_OP");
            $$.ast = obj;

            std::string t1 = $1.type, t2 = $3.type;
            if((t1 == "int") && (t2 == "int")) {
               // Valid
            }
            else if(baseType(t1) == baseType(t2)) {
               if(baseType(t1).substr(0, 6) == "struct" && depth(t1) == 0) {
                  error(@1, "Invalid operands types for binary >=, \"" + t1 + "\" and \"" + t2 + "\"");
               }
               else if(depth(t1) != depth(t2)) {
                  error(@1, "Invalid operands types for binary >=, \"" + t1 + "\" and \"" + t2 + "\"");
               }
               else {
                  if(isArray(t1) && isArray(t2)) {
                     if(modType(t1) == modType(t2)) {
                        // Valid
                     }
                     else {
                        error(@1, "Invalid operands types for binary >=, \"" + t1 + "\" and \"" + t2 + "\"");
                     }
                  }
                  else if(isPointer(t1) && isPointer(t2)) {
                     // Valid because same depth
                  }
                  else {
                     if(modType(t1) == modType(t2)) {
                        // Valid
                     }
                     else {
                        error(@1, "Invalid operands types for binary >=, \"" + t1 + "\" and \"" + t2 + "\"");
                     }
                  }
               }
            }
            else {
               error(@1, "Invalid operands types for binary >=, \"" + t1 + "\" and \"" + t2 + "\"");
            }

            if(isInt(t1)&& isInt(t2)) {
               // *(obj->op) += "_INT";

               $$.type = "int";
            }
            else {
               // Error
            }

            obj->type = $$.type;
            $$.text = $1.text + ">=" + $3.text;
            $$.lval = false;
         }
         ;

additive_expression:
         multiplicative_expression
         {
            $$.ast = $1.ast;
            $$.type = $1.type;
            $$.text = $1.text;
            $$.lval = $1.lval;
         }
         | additive_expression '+' multiplicative_expression
         {
            op_binary_astnode* obj = new op_binary_astnode();
            obj->lExp = (exp_astnode *) $1.ast;
            obj->rExp = (exp_astnode *) $3.ast;
            obj->op = new std::string("PLUS");
            $$.ast = obj;

            std::string t1 = $1.type, t2 = $3.type;
            if(!(t1 == "int") && !(t2 == "int")) {
               // Invalid

               error(@1, "Invalid operand types for binary +, \"" + t1 + "\" and \"" + t2 + "\"");
            }
            else if((t1 == "int") && (t2 == "int")) {
               // Valid

               $$.type = "int";
            }
            else if(t1 == "int" || t2 == "int") {
               // Valid

               if(t1 == "string" || t2 == "string") {
                  error(@1, "Invalid operand types for binary +, \"" + t1 + "\" and \"" + t2 + "\"");
               }

               if(t1 == "int") $$.type = t2;
               else $$.type = t1;
            }
            else {
               // Invalid

               error(@1, "Invalid operand types for binary +, \"" + t1 + "\" and \"" + t2 + "\"");
            }

            if(isInt(t1)&& isInt(t2)) {
               // *(obj->op) += "_INT";
            }
            else {
               // Error
            }

            obj->type = $$.type;
            $$.text = $1.text + "+" + $3.text;
            $$.lval = false;
         }
         | additive_expression '-' multiplicative_expression
         {
            op_binary_astnode* obj = new op_binary_astnode();
            obj->lExp = (exp_astnode *) $1.ast;
            obj->rExp = (exp_astnode *) $3.ast;
            obj->op = new std::string("MINUS");
            $$.ast = obj;

            std::string t1 = $1.type, t2 = $3.type;
            if((t1 == "int") && (t2 == "int")) {
               // Valid

               $$.type = "int";
            }
            else if(isInt(t1) && t2 == "int") {
               // Valid

               $$.type = t1;
            }
            else if(modType(t1) == modType(t2)) {
               // Valid

               $$.type = "int";
            }
            else {
               // Invalid

               error(@1, "Invalid operand types for binary -, \"" + t1 + "\" and \"" + t2 + "\"");
            }

            if(isInt(t1)&& isInt(t2)) {
               // *(obj->op) += "_INT";
            }
            else {
               // Error
            }

            obj->type = $$.type;
            $$.text = $1.text + "-" + $3.text;
            $$.lval = false;
         }
         ;

unary_expression:
         postfix_expression
         {
            $$.ast = $1.ast;
            $$.type = $1.type;
            $$.text = $1.text;
            $$.lval = $1.lval;
         }
         | unary_operator unary_expression
         {
            op_unary_astnode* obj = new op_unary_astnode();
            obj->op = new std::string($1.text);
            obj->exp = (exp_astnode *) $2.ast;
            $$.ast = obj;

            if($1.text == "UMINUS") {
               // Assuming only numbers as the type of
               // unary_expression

               if(!($2.type == "int")) {
                  error(@1, "Operand of unary - should be an int or float");
               }
               $$.type = $2.type;

               $$.lval = false;
            }
            else if($1.text == "NOT") {
               // Operand of NOT should be int, float or pointer or array

               if(!(isInt($2.type))) {
                  error(@1, "Operand of NOT should be an int or float or pointer");
               }

               // Need to handle the errors here
               $$.type = "int";

               $$.lval = false;
            }
            else if($1.text == "ADDRESS") {
               // Check till the first character that is not a letter
               $$.lval = false;

               if(!$2.lval) {
                  error(@1, "Operand of & should  have lvalue");
               }

               int ind = $2.type.find('[');
               if(ind == (int) std::string::npos) {
                  $$.type = $2.type + "*";
               }
               else {
                  $$.type = $2.type.substr(0, ind);
                  $$.type += "(*)";
                  $$.type += $2.type.substr(ind);
               }
            }
            else if($1.text == "DEREF") {
               // Check till the first character that is not a letter

               if($2.type == "void*" || $2.type == "void(*)" || !(isInt($2.type) && !($2.type == "int"))) {
                  // Not an array or pointer

                  error(@1, "Invalid operand type \"" + $2.type + "\" of unary *");
               }
               
               // Confirmed that it is an array or a pointer
               int ind = $2.type.find(")");
               if(ind == (int) std::string::npos) {
                  // No group simple case

                  int open_ind = (int) $2.type.find('[');
                  if(open_ind == (int) std::string::npos) {
                     $$.type = $2.type.substr(0, (int) $2.type.length()-1);
                  }
                  else {
                     // Got an array

                     int close_ind = (int) $2.type.find(']', open_ind);

                     $$.type = $2.type.substr(0, open_ind);
                     if(close_ind+1 < (int) $2.type.length()) {
                        $$.type += $2.type.substr(close_ind + 1);
                     }
                  }
               }
               else {
                  // Have one group

                  int open_ind = (int) $2.type.find('(');
                  int close_ind = (int) $2.type.find(')', open_ind);

                  $$.type = $2.type.substr(0, open_ind);
                  if(close_ind+1 < (int) $2.type.length()) {
                     $$.type += $2.type.substr(close_ind + 1);
                  }
               }
               $$.lval = true;
            }

            obj->type = $$.type;
            $$.text = $1.text + $2.text;
         }
         ;

multiplicative_expression:
         unary_expression
         {
            $$.ast = $1.ast;
            $$.type = $1.type;
            $$.text = $1.text;
            $$.lval = $1.lval;
         }
         | multiplicative_expression '*' unary_expression
         {
            op_binary_astnode* obj = new op_binary_astnode();
            obj->lExp = (exp_astnode *) $1.ast;
            obj->rExp = (exp_astnode *) $3.ast;
            obj->op = new std::string("MULT");
            $$.ast = obj;

            std::string t1 = $1.type, t2 = $3.type;

            if((t1 == "int") && (t2 == "int")) {
               // Valid
            }
            else {
               // Invalid

               error(@1, "Invalid operand types for binary *, \"" + t1 + "\" and \"" + t2 + "\"");
            }

            if(isInt(t1)&& isInt(t2)) {
               // *(obj->op) += "_INT";

               $$.type = "int";
            }
            else {
               // Error
            }

            obj->type = $$.type;
            $$.text = $1.text + "*" + $3.text;
            $$.lval = false;
         }
         | multiplicative_expression '/' unary_expression
         {
            op_binary_astnode* obj = new op_binary_astnode();
            obj->lExp = (exp_astnode *) $1.ast;
            obj->rExp = (exp_astnode *) $3.ast;
            obj->op = new std::string("DIV");
            $$.ast = obj;

            std::string t1 = $1.type, t2 = $3.type;

            if((t1 == "int") && (t2 == "int")) {
               // Valid
            }
            else {
               // Invalid

               error(@1, "Invalid operand types for binary *, \"" + t1 + "\" and \"" + t2 + "\"");
            }

            if(isInt(t1)&& isInt(t2)) {
               // *(obj->op) += "_INT";

               $$.type = "int";
            }
            else {
               // Error
            }

            obj->type = $$.type;
            $$.text = $1.text + "/" + $3.text;
            $$.lval = false;
         }
         ;

postfix_expression:
         primary_expression
         {
            $$.ast = $1.ast;
            $$.type = $1.type;
            $$.text = $1.text;
            $$.lval = $1.lval;
         }
         | postfix_expression '[' expression ']'
         {
            arrayref_astnode* obj = new arrayref_astnode();
            obj->lExp = (exp_astnode *) $1.ast;
            obj->rExp = (exp_astnode *) $3.ast;
            $$.ast = obj;

            // should be an error if expression's type is not
            // dereferencable

            if($3.type != "int") {
               error(@1, "Array subscript is not an integer");
            }

            if(isInt($1.type) && !($1.type == "int")) {}
            else {
               error(@1, "Subscripted value is neither array nor pointer");
            }

            // Will surely be an array or pointer

            int ind = $1.type.find(")");
            if(ind == (int) std::string::npos) {
               // No group simple case

               int open_ind = (int) $1.type.find('[');
               if(open_ind == (int) std::string::npos) {
                  $$.type = $1.type.substr(0, (int) $1.type.length()-1);
               }
               else {
                  // Got an array

                  int close_ind = (int) $1.type.find(']', open_ind);

                  $$.type = $1.type.substr(0, open_ind);
                  if(close_ind+1 < (int) $1.type.length()) {
                     $$.type += $1.type.substr(close_ind + 1);
                  }
               }
            }
            else {
               // Have one group

               int open_ind = (int) $1.type.find('(');
               int close_ind = (int) $1.type.find(')', open_ind);

               $$.type = $1.type.substr(0, open_ind);
               if(close_ind+1 < (int) $1.type.length()) {
                  $$.type += $1.type.substr(close_ind + 1);
               }
            }
            
            obj->type = $$.type;
            $$.text = $1.text + "[" + $3.text + "]";
            $$.lval = true;
         }
         | IDENTIFIER '(' ')'
         {
            if(($1 != "printf" && $1 != "scanf") && gst->tab.find($1) == gst->tab.end()) {
               // Error : Not declared

               error(@1, "Procedure \"" + $1 + "\" not declared");
            }
            else {
               funcall_astnode* obj = new funcall_astnode();
               identifier_astnode* id = new identifier_astnode();
               id->name = new std::string($1);
               id->st = gst;
               obj->fname = id;
               $$.ast = obj;

               $$.type = gst->tab[$1]->type;

               if($1 != "printf" && $1 != "scanf") {
                  int args = 0;
                  std::map<std::string, stRecord*> tab = gst->tab[$1]->symtab->tab;

                  for(auto it = tab.begin(); it != tab.end(); ++it) {
                     if(it->second->param_local == "param") {
                        args++;
                     }
                  }

                  if(args > 0) {
                     // Error : Too few arguments

                     error(@1, "Procedure \"" + $1 + "\" called with too few arguments");
                  }

                  if(gst->tab[$1]->var_func != "fun") {
                     // Error : Not a function

                     error(@1, "Called object \"" + $1 + "\" is not a procedure");
                  }
               }

               obj->type = $$.type;
               $$.text = $1 + "(" + ")";
            }

            $$.lval = false;
         }
         | IDENTIFIER '('
         M
         expression_list ')'
         {
            if(($1 != "printf" && $1 != "scanf") && gst->tab.find($1) == gst->tab.end()) {
               // Error : Not declared

               error(@1, "Procedure \"" + $1 + "\" not declared");
            }
            if(gst->tab[$1]->var_func != "fun") {
               // Error : Not a function

               error(@1, "Called object \"" + $1 + "\" is not a procedure");
            }
            
            funcall_astnode* obj = new funcall_astnode();
            identifier_astnode* id = new identifier_astnode();
            id->name = new std::string($1);
            id->st = gst;
            obj->fname = id;
            obj->callExps = *(std::vector<exp_astnode*> *) actPtr;
            $$.ast = obj;

            actPtr = actVx.top();
            actVx.pop();

            if($1 != "printf" && $1 != "scanf") {
               int args = 0;
               std::map<std::string, stRecord*> tab = gst->tab[$1]->symtab->tab;

               for(auto it = tab.begin(); it != tab.end(); ++it) {
                  if(it->second->param_local == "param") {
                     args++;
                  }
               }

               if(args > (int) obj->callExps.size()) {
                  // Error : Too few arguments

                  error(@1, "Procedure \"" + $1 + "\" called with too few arguments");
               }
               else if(args < (int) obj->callExps.size()) {
                  // Error : Too many arguments

                  error(@1, "Procedure \"" + $1 + "\" called with too many arguments");
               }
            }

            if($1 != "printf" && $1 != "scanf") {
               // See the lst of this function
               // Have a look at the parameters and sort by offsets
               // Store their types and then type cast the expressions

               std::vector<std::pair<int, std::string>> params;
               struct SymTab st = *(gst->tab[$1]->symtab);

               for(auto it = st.tab.begin(); it != st.tab.end(); ++it) {
                  stRecord entry = *(it->second);
                  if(entry.offset > 0)
                     params.push_back({entry.offset, entry.type});
               }

               sort(params.begin(), params.end(), std::greater<std::pair<int, std::string>>());

               for(int i = 0; i < (int) params.size(); i++) {
                  // exp_astnode* exp = obj->callExps[i];
                  std::string t1 = params[i].second, t2 = $4.type_list[i];

                  if(baseType(t1) == "void") {
                     if(depth(t1) == 1) {
                        if(depth(t2) > 0) {
                           // Valid
                        }
                        else {
                           // Invalid

                           error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                        }
                     }
                     else {
                        if(baseType(t2) == "void" && depth(t1) == depth(t2)) {
                           // Valid
                           std::cout << t1 << " and " << t2 << std::endl;
                        }
                        else {
                           error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                        }
                     }
                  }
                  else if(depth(t1) > 0) {
                     if(isArray(t1)) {
                        if(depth(t1) == 1) {
                           if(depth(t2) == 1) {
                              // Valid
                           }
                           else {
                              // Invalid

                              error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                           }
                        }
                        else {
                           int open_ind = t1.find('[');
                           int close_ind = t1.find(']', open_ind);
                           
                           std::string type = t1.substr(0, open_ind);
                           type += "(*)";
                           type += t1.substr(close_ind + 1);

                           if(type[type.length() - 1] == ')') {
                              type = t1.substr(0, open_ind) + "*";
                           }

                           if(depth(t2) != depth(t1)) {
                              // Invalid

                              error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                           }
                           else {
                              // Valid

                              if(isArray(t2)) {
                                 int open_ind = t2.find('[');
                                 int close_ind = t2.find(']', open_ind);
                                 
                                 std::string t2type = t2.substr(0, open_ind);
                                 t2type += "(*)";
                                 t2type += t2.substr(close_ind + 1);

                                 if(t2type[t2type.length() - 1] == ')') {
                                    t2type = t2.substr(0, open_ind) + "*";
                                 }

                                 if(t2type == type) {
                                    // Valid
                                 }
                                 else {
                                    // Invalid
                                    
                                    error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                                 }
                              }
                              else {
                                 if(t2 == type) {
                                    // Valid
                                 }
                                 else {
                                    // Invalid
                                    
                                    error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                                 }
                              }

                              error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                           }
                        }
                     }
                     else {
                        // Non arrays
                        // Can only be simple pointers of int, float or struct

                        if(baseType(t2) == "void") {
                           if(depth(t2) == 1) {
                              // Valid
                           }
                           else {
                              // Invalid

                              error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                           }
                        }
                        else {
                           if(isArray(t2)) {
                              // Match the converted type

                              int open_ind = t2.find('[');
                              int close_ind = t2.find(']', open_ind);
                              
                              std::string t2type = t2.substr(0, open_ind);
                              t2type += "(*)";
                              t2type += t2.substr(close_ind + 1);

                              if(t2type[t2type.length() - 1] == ')') {
                                 t2type = t2.substr(0, open_ind) + "*";
                              }

                              if(t2type == t1) {
                                 // Valid
                              }
                              else {
                                 // Invalid
                                 
                                 error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                              }
                           }
                           else {
                              if(std::count(t2.begin(), t2.end(), ')') > 0) {
                                 // Invalid

                                 error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                              }
                              else if(t1 == t2) {
                                 // Valid
                              }
                              else {
                                 // Invalid

                                 error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                              }
                           }
                        }
                     }
                  }
                  else {
                     // Base type in LHS
                     if(depth(t2) != 0) {
                        // Invalid

                        error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                     }
                     else if(t1.substr(0, 6) == "struct") {
                        if(t1 == t2) {
                           // Valid
                        }
                        else {
                           // Invalid

                           error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                        }
                     }
                     else if(t1 == "int") {
                        if(t2 != t1) {
                           // Invalid

                           error(@1, "Expected \"" + t1 + "\" but argument is of type \"" + t2 + "\"");
                        }
                     }
                     else {
                        // Never occurs
                     }
                  }
               }
               $$.type = gst->tab[$1]->type;
            }
            else {
               // Dummy return value
               // change it later
               $$.type = "void";
            }

            obj->type = $$.type;
            $$.text = $1 + "(" + $4.text + ")";
            $$.lval = false;
         }
         | postfix_expression '.' IDENTIFIER
         {
            member_astnode* obj = new member_astnode();
            obj->exp = (exp_astnode *) $1.ast;
            
            identifier_astnode* id = new identifier_astnode();
            id->name = new std::string($3);
            obj->id = id;
            $$.ast = obj;

            // Type will work only when the postfix_expression
            // is a struct

            if($1.type.substr(0, 6) == "struct") {
               // Some struct derivative
               if(gst->tab.find($1.type) == gst->tab.end()) {
                  // Not a struct
                  error(@1, "Left operand of \".\" is not a structure");
               }
               
               // Is a struct present in the gst
               // Find if $3 is in the lst
               if(gst->tab[$1.type]->symtab->tab.find($3) == gst->tab[$1.type]->symtab->tab.end()) {
                  error(@1, "Struct \"" + $1.type + "\" has no member named \"" + $3 + "\"");
               }

               id->st = gst->tab[$1.type]->symtab;
               $$.type = gst->tab[$1.type]->symtab->tab[$3]->type;

               $$.text = $1.text + "." + $3;
            }

            obj->type = $$.type;
            $$.lval = true;
         }
         | postfix_expression PTR_OP IDENTIFIER
         {
            arrow_astnode* obj = new arrow_astnode();
            obj->exp = (exp_astnode *) $1.ast;
            
            identifier_astnode* id = new identifier_astnode();
            id->name = new std::string($3);

            obj->id = id;
            $$.ast = obj;

            // Find out the struct from the pointer

            if($1.type.substr(0, 6) == "struct") {
               if(depth($1.type)) {
                  // Is a pointer or array to a struct
                  // Need to remove the * or (*) or [] from the type

                  int ind = $1.type.find(")");
                  if(ind == (int) std::string::npos) {
                     // No group simple case

                     int open_ind = (int) $1.type.find('[');
                     if(open_ind == (int) std::string::npos) {
                        $$.type = $1.type.substr(0, (int) $1.type.length()-1);
                     }
                     else {
                        // Got an array

                        int close_ind = (int) $1.type.find(']', open_ind);

                        $$.type = $1.type.substr(0, open_ind);
                        if(close_ind+1 < (int) $1.type.length()) {
                           $$.type += $1.type.substr(close_ind + 1);
                        }
                     }
                  }
                  else {
                     // Have one group

                     int open_ind = (int) $1.type.find('(');
                     int close_ind = (int) $1.type.find(')', open_ind);

                     $$.type = $1.type.substr(0, open_ind);
                     if(close_ind+1 < (int) $1.type.length()) {
                        $$.type += $1.type.substr(close_ind + 1);
                     }
                  }

                  std::string typ = $$.type;

                  if(gst->tab[typ]->symtab->tab.find($3) == gst->tab[typ]->symtab->tab.end()) {
                     error(@1, "Struct \"" + typ + "\" has no member named " + $3);
                  }

                  id->st = gst->tab[typ]->symtab;
                  $$.type = gst->tab[typ]->symtab->tab[$3]->type;
                  $$.text = $1.text + "->" + $3;
               }
               else {
                  error(@1, "Left operand of \"->\" is not a pointer to structure");
               }
            }

            obj->type = $$.type;
            $$.lval = true;
         }
         | postfix_expression INC_OP
         {
            op_unary_astnode* obj = new op_unary_astnode();
            obj->op = new std::string("PP");
            obj->exp = (exp_astnode *) $1.ast;
            $$.ast = obj;

            $$.type = $1.type;
            obj->type = $$.type;
            $$.text = $1.text + $2;

            if((isInt($1.type) && !isArray($1.type))) {
               // Good
            }
            else {
               error(@1, "Operand of \"++\" should be a int, float or pointer");
            }

            if(!$1.lval) {
               error(@1, "Operand of \"++\" should have lvalue");
            }

            $$.lval = false;
         }
         ;

primary_expression:
         IDENTIFIER
         {
            SymTab* lst = tables.top();
            if(lst->tab.find($1) != lst->tab.end()) {
               identifier_astnode* obj = new identifier_astnode();
               obj->st = lst;
               obj->name = new std::string($1);
               $$.ast = obj;

               $$.type = lst->tab[$1]->type;
               obj->type = $$.type;
               $$.text = $1;

               $$.lval = true;
            }
            else {
               // Error : Variable not declared

               error(@1, "Variable \"" + $1 + "\" not declared");
            }
         }
         | INT_CONSTANT
         {
            intconst_astnode* obj = new intconst_astnode();
            obj->num = new std::string($1);
            $$.ast = obj;
            $$.type = "int";
            obj->type = $$.type;
            $$.text = $1;
            $$.lval = false;
         }
         | '(' expression ')'
         {
            $$.ast = $2.ast;
            $$.type = $2.type;
            $$.text = $2.text;
            $$.lval = $2.lval;
         }
         ;

expression_list:
         expression
         {
            ((std::vector<exp_astnode*> *) actPtr)->push_back((exp_astnode *) $1.ast);
            $$.ast = $1.ast;

            $$.type_list.push_back($1.type);
         }
         | expression_list ',' expression
         {
            ((std::vector<exp_astnode*> *) actPtr)->push_back((exp_astnode *) $3.ast);
            $$.ast = $1.ast;

            $$.type_list = $1.type_list;
            $$.type_list.push_back($3.type);
         }
         ;

unary_operator:
         '-'
         {
            $$.text = "UMINUS";
         }
         | '!'
         {
            $$.text = "NOT";
         }
         | '&'
         {
            $$.text = "ADDRESS";
         }
         | '*'
         {
            $$.text = "DEREF";
         }
         ;

selection_statement:
         IF '(' expression ')' statement ELSE statement
         {
            if_astnode* obj = new if_astnode();
            obj->cond = (exp_astnode *) $3.ast;
            obj->ifStmt = (statement_astnode *) $5.ast;
            obj->elseStmt = (statement_astnode *) $7.ast;
            $$.ast = obj;

            $$.ret = $5.ret || $7.ret;
         }
         ;

iteration_statement:
         WHILE '(' expression ')' statement
         {
            while_astnode* obj = new while_astnode();
            obj->cond = (exp_astnode *) $3.ast;
            obj->stmt = (statement_astnode *) $5.ast;
            $$.ast = obj;

            $$.ret = $5.ret;
         }
         | FOR '(' assignment_expression ';' expression ';' assignment_expression ')' statement
         {
            for_astnode* obj = new for_astnode();
            obj->init = (exp_astnode *) $3.ast;
            obj->guard = (exp_astnode *) $5.ast;
            obj->iter = (exp_astnode *) $7.ast;
            obj->stmt = (statement_astnode *) $9.ast;
            $$.ast = obj;

            $$.ret = $9.ret;
         }
         ;

declaration_list:
         declaration
         {
            // declaration handles the required stuff

            $$.size = $1.size;
         }
         | declaration_list declaration
         {
            // declaration handles the required stuff

            $$.size = $1.size + $2.size;
         }
         ;

declaration:
         type_specifier declarator_list ';'
         {
            // Error if type_specifier is "void"


            // Now modify all of the vars in curr_varlist
            int num_vars = curr_varlist.size();

            SymTab* lst = tables.top();

            $$.size = 0;

            for(int i = 0; i < num_vars; i++) {
               if(lst->tab[curr_varlist[i]]->type[0] != '*') {
                  if($1.text == "void") {
                     error(@1, "Cannot declare variable of type \"void\"");
                  }
                  else if($1.text == curr_unit) {
                     error(@1, "\"" + $1.text + "\" is not defined");
                  }
                  
                  lst->tab[curr_varlist[i]]->size = lst->tab[curr_varlist[i]]->size*$1.size;
               }
               lst->tab[curr_varlist[i]]->type = $1.text + lst->tab[curr_varlist[i]]->type;
               lst->tab[curr_varlist[i]]->param_local = "local";
               
               if(struct_func) {
                  lst->tab[curr_varlist[i]]->offset = lst->curr_offset;
                  lst->curr_offset += lst->tab[curr_varlist[i]]->size;
               }
               else {
                  lst->curr_offset -= lst->tab[curr_varlist[i]]->size;
                  lst->tab[curr_varlist[i]]->offset = lst->curr_offset;
               }

               $$.size += lst->tab[curr_varlist[i]]->size;
            }

            curr_varlist.clear();
         }
         ;

declarator_list:
         declarator
         {
            // declarator has already created the record
            // and initialized parts of the type
            // My job is to add the type_specifier to every
            // one of the variables in the list

            curr_varlist.push_back($1.text);

            // Default action
            $$ = *new attrib();
         }
         | declarator_list ',' declarator
         {
            // declarator has already created the record
            // and initialized parts of the type
            // My job is to add the type_specifier to every
            // one of the variables in the list

            curr_varlist.push_back($3.text);

            // Default action
            $$ = *new attrib();
         }
         ;
%%
void IPL::Parser::error( const location_type &l, const std::string &err_message )
{
   std::cout << "Error at line " << l.begin.line << ": " << err_message << "\n";
   exit(1);
}