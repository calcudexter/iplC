#include <vector>
#include <map>
#include <string>
#include <utility>
#include <stack>
#include <iostream>
#include <stdarg.h>
#include <algorithm>

enum typeExp {
    empty_astN,
    seq_astN,
    assignS_astN,
    return_astN,
    if_astN,
    while_astN,
    for_astN,
    proccall_astN,
    identifier_astN,
    arrayref_astN,
    member_astN,
    arrow_astN,
    op_binary_astN,
    op_unary_astN,
    assignE_astN,
    funcall_astN,
    intconst_astN,
    stringconst_astN
};

extern void printAst(const char *astname, const char *fmt, ...);

class abstract_astnode
{
public:
    virtual void print(int blanks) = 0;
    enum typeExp astnode_type;
    std::string type;
};

class statement_astnode : public abstract_astnode {
public:
    void print(int blanks) {

    }
};
class exp_astnode : public abstract_astnode {
public:
    void print(int blanks) {
        
    }
};
class ref_astnode : public exp_astnode {
public:
    void print(int blanks) {
        
    }
};

class identifier_astnode;

class empty_astnode : public statement_astnode {
public:
    
    void print(int blanks) {
        std::cout << "\"empty\"" << std::endl;
    }

    empty_astnode() {
        astnode_type = empty_astN;
    }
};

class seq_astnode : public statement_astnode {
public:
    std::vector<statement_astnode*> stmts;

    // void print(int blanks) {
    //     printAst("seq", "l", &stmts);
    // }

    void print(int blanks) {
        std::cout << "{" << std::endl;
        std::cout << "\"seq\": ";
        std::cout << "[" << std::endl;
        int len = stmts.size();

        for(int i = 0; i < len; i++) {
            stmts[i]->print(0);
            if(i < len-1)
                std::cout << "," << std::endl;
            else
                std::cout << std::endl;
        }
        std::cout << "]" << std::endl;
        std::cout << "}" << std::endl;
    }

    seq_astnode() {
        astnode_type = seq_astN;
    }
};

class assignS_astnode : public statement_astnode {
public:
    exp_astnode *lExp, *rExp;

    void print(int blanks) {
        printAst("assignS", "aa", lExp, "left", rExp, "right");
    }

    assignS_astnode() {
        astnode_type = assignS_astN;
    }
};

class return_astnode : public statement_astnode {
public:
    exp_astnode *retExp;
    std::string *ret_type;
    std::string *fun_name;
    // void print(int blanks) {
    //     printAst("return", "a", retExp, "return");
    // }

    void print(int blanks) {
        std::cout << "{" << std::endl;
        std::cout << "\"return\": " << std::endl;
        retExp->print(0);
        std::cout << "}" << std::endl;
    }

    return_astnode() {
        astnode_type = return_astN;
    }
};

class proccall_astnode : public statement_astnode {
public:
    identifier_astnode* fname;
    std::vector<exp_astnode*> callExps;

    void print(int blanks) {
        printAst("proccall", "al", fname, "fname", callExps, "params");
    }

    proccall_astnode() {
        astnode_type = proccall_astN;
    }
};

class if_astnode : public statement_astnode {
public:
    exp_astnode *cond;
    statement_astnode *ifStmt, *elseStmt;

    void print(int blanks) {
        printAst("if", "aaa", cond, "cond", ifStmt, "then", elseStmt, "else");
    }

    if_astnode() {
        astnode_type = if_astN;
    }
};

class while_astnode : public statement_astnode {
public:
    exp_astnode *cond;
    statement_astnode *stmt;

    void print(int blanks) {
        printAst("while", "aa", cond, "cond", stmt, "stmt");
    }

    while_astnode() {
        astnode_type = while_astN;
    }
};

class for_astnode : public statement_astnode {
public:
    exp_astnode *init, *guard, *iter;
    statement_astnode *stmt;

    void print(int blanks) {
        printAst("for", "aaaa", init, "init", guard, "guard", iter, "step", stmt, "body");
    }

    for_astnode() {
        astnode_type = for_astN;
    }
};


class member_astnode : public ref_astnode {
public:
    exp_astnode *exp;
    identifier_astnode *id;

    void print(int blanks) {
        printAst("member", "aa", exp, "struct", id, "field");
    }

    member_astnode() {
        astnode_type = member_astN;
    }
};

class arrow_astnode : public ref_astnode {
public:
    exp_astnode *exp;
    identifier_astnode *id;

    void print(int blanks) {
        printAst("arrow", "aa", exp, "pointer", id, "field");
    }

    arrow_astnode() {
        astnode_type = arrow_astN;
    }
};

class identifier_astnode : public ref_astnode {
public:
    std::string *name;

    // Will be nullptr for printf
    struct SymTab* st;

    // void print(int blanks) {
    //     printAst("identifier", "s", name->c_str(), "identifier");
    // }

    void print(int blanks) {
        std::cout << "{" << std::endl;
        std::cout << "\"identifier\": ";
        std::cout << "\"" << name->c_str() << "\"" << std::endl;
        std::cout << "}" << std::endl;
    }

    identifier_astnode() {
        astnode_type = identifier_astN;
    }
};

class arrayref_astnode : public ref_astnode {
public:
    exp_astnode *lExp, *rExp;

    void print(int blanks) {
        printAst("arrayref", "aa", lExp, "array", rExp, "index");
    }

    arrayref_astnode() {
        astnode_type = arrayref_astN;
    }
};

class op_binary_astnode : public exp_astnode {
public:
    std::string *op;
    exp_astnode *lExp, *rExp;

    void print(int blanks) {
        printAst("op_binary", "saa", op->c_str(), "op", lExp, "left", rExp, "right");
    }

    op_binary_astnode() {
        astnode_type = op_binary_astN;
    }
};

class op_unary_astnode : public exp_astnode {
public:
    std::string *op;
    exp_astnode *exp;

    void print(int blanks) {
        printAst("op_unary", "sa", op->c_str(), "op", exp, "child");
    }

    op_unary_astnode() {
        astnode_type = op_unary_astN;
    }
};

class assignE_astnode : public exp_astnode {
public:
    exp_astnode *lExp, *rExp;

    void print(int blanks) {
        printAst("assignE", "aa", lExp, "left", rExp, "right");
    }

    assignE_astnode() {
        astnode_type = assignE_astN;
    }
};

class funcall_astnode : public exp_astnode {
public:
    identifier_astnode* fname;
    std::vector<exp_astnode*> callExps;

    void print(int blanks) {
        printAst("funcall", "al", fname, "fname", callExps, "params");
    }

    funcall_astnode() {
        astnode_type = funcall_astN;
    }
};

class intconst_astnode : public exp_astnode {
public:
    std::string* num;

    // void print(int blanks) {
    //     printAst("intconst", "i", num->c_str(), "num");
    // }

    void print(int blanks) {
        std::cout << "{" << std::endl;
        std::cout << "\"intconst\": ";

        int n = std::stoi(num->c_str());
        std::cout << n;
        std::cout << "}" << std::endl;
    }

    intconst_astnode() {
        astnode_type = intconst_astN;
    }
};

class string_astnode : public exp_astnode {
public:
    std::string *str;

    // void print(int blanks) {
    //     printAst("string", "s", str->c_str(), "string");
    // }

    void print(int blanks) {
        std::cout << "{" << std::endl;
        std::cout << "\"stringconst\": ";
        std::cout << str->c_str() << std::endl;
        std::cout << "}" << std::endl;
    }

    string_astnode() {
        astnode_type = stringconst_astN;
    }
};

struct SymTab;

// Code regarding symbol table
struct stRecord {
    std::string symname;
    std::string var_func;
    std::string param_local;
    std::string type;
    int size;
    int offset;
    struct SymTab* symtab;
};

struct SymTab {
    std::map<std::string, struct stRecord*> tab;
    int curr_offset;
    void print() {
        std::cout << "[" << std::endl;
        for(auto it = tab.begin(); it != tab.end(); ++it) {
            stRecord* entry = it->second;
            std::cout << "[\t";
            std::cout << "   \"" << entry->symname << "\",\t";
            std::cout << "   \"" << entry->var_func << "\",\t";
            std::cout << "   \"" << entry->param_local << "\",\t";
            std::cout << "   " << entry->size << ",\t";
            std::cout << "   " << entry->offset << ",\t";
            std::cout << "   \"" << entry->type << "\"" << std::endl;
            std::cout << "]" << std::endl;
            if(next(it, 1) != tab.end())
                std::cout << "," << std::endl;
            else
                std::cout << std::endl;
        }
        std::cout << "]" << std::endl;
    }
    void printgst() {
        std::cout << "[";
        for(auto it = tab.begin(); it != tab.end(); ++it) {
            stRecord* entry = it->second;
            std::cout << "[\t";
            std::cout << "   \"" << entry->symname << "\",\t";
            std::cout << "   \"" << entry->var_func << "\",\t";
            std::cout << "   \"" << entry->param_local << "\",\t";
            std::cout << "   " << entry->size << ",\t";

            if(entry->var_func == "struct") {
                std::cout << "   \"-\",\t";
                std::cout << "   \"-\"" << std::endl;;
            }
            else if(entry->var_func == "fun") {
                std::cout << "   " << entry->offset << ",\t";
                std::cout << "   \"" << entry->type << "\"" << std::endl;
            }

            std::cout << "]";
            if(next(it, 1) != tab.end())
                std::cout << "," << std::endl;
            else
                std::cout << std::endl;
        }
        std::cout << "]" << std::endl;
    }
};

struct attrib {
    std::string text;
    std::string var;
    std::string type;
    std::vector<std::string> type_list;
    bool lval;

    int size;
    struct SymTab* symtab;

    abstract_astnode* ast;
    bool non_ret;
    bool ret;
};