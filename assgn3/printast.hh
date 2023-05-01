extern std::string code;
extern std::string locals;
extern int cntLocl, cntLabl;
extern std::stack<struct SymTab*> tables;
extern std::stack<std::string> rstack;
extern SymTab* gst;

int L(abstract_astnode* obj);
bool isLeaf(abstract_astnode* obj);
void genCode(abstract_astnode* obj);
void gen(std::string arg, ...);

void initRstack() {
	rstack.push("%edi");
	rstack.push("%esi");
	rstack.push("%edx");
	rstack.push("%ecx");
	rstack.push("%ebx");
	rstack.push("%eax");
}

std::string getL(std::string reg) {
	if(reg == "%eax") return "%al";
	if(reg == "%ebx") return "%bl";
	if(reg == "%ecx") return "%cl";
	if(reg == "%edx") return "%dl";
	return "%nl";
}

bool isReg(std::string str) {
	if(str == "%eax") return true;
	if(str == "%ebx") return true;
	if(str == "%ecx") return true;
	if(str == "%edx") return true;
	if(str == "%esi") return true;
	if(str == "%edi") return true;
	return false;
}

bool isMem(std::string str) {
	if(str.find("(%ebp)") != std::string::npos) return true;
	return false;
}

bool isFree(std::string reg) {
	std::stack<std::string> rstack_copy = rstack;

	while(!rstack_copy.empty()) {
		std::string top = rstack_copy.top();
		rstack_copy.pop();

		if(top == reg) return true;
	}

	return false;
}

void swap(std::stack<std::string> &stack) {
	std::string top = stack.top();
	stack.pop();

	std::string nxt = stack.top();
	stack.pop();

	stack.push(top);
	stack.push(nxt);
}

// Register stack is eax, ebx, ecx, edx, esi, edi
// After exhausting the registers, I will need to use the main memory
// as a stack
// Don't know what to do of the tstack rn but fuck it


// fmt is a format string that tells about the type of the arguments.
void printAst(const char *astname, const char *fmt, ...)
{   
	typedef std::vector<abstract_astnode *>* pv;
	va_list args;
	va_start(args, fmt);
	if ((astname != NULL) && (astname[0] != '\0'))
	{
		std::cout << "{ ";
		std::cout << "\"" << astname << "\"" << ": ";
	}
    std::cout << "{" << std::endl;
	while (*fmt != '\0')
	{
		if (*fmt == 'a')
		{
			abstract_astnode *a = va_arg(args, abstract_astnode *);
			char * field = va_arg(args, char *);
			std::cout << "\"" << field << "\": " << std::endl;
			
			a->print(0);
		}
		else if (*fmt == 's')
		{
			char *str = va_arg(args, char *);
			char * field = va_arg(args, char *);
			std::cout << "\"" << field << "\": ";

			std::cout << "\"" << str << "\"" << std::endl;
		}
		else if (*fmt == 'i')
		{
			int i = va_arg(args, int);
			char * field = va_arg(args, char *);
			std::cout << "\"" << field << "\": ";

			std::cout << i;
		}
		else if (*fmt == 'f')
		{
			double f = va_arg(args, double);
			char * field = va_arg(args, char *);
			std::cout << "\"" << field << "\": ";
			std::cout << f;
		}
		else if (*fmt == 'l')
		{
			pv f =  va_arg(args, pv);
			char * field = va_arg(args, char *);
			std::cout << "\"" << field << "\": ";
			std::cout << "[" << std::endl;
			for (int i = 0; i < (int)f->size(); ++i)
			{
				(*f)[i]->print(0);
				if (i < (int)f->size() - 1)
					std::cout << "," << std::endl;
				else
					std::cout << std::endl;
			}
			std::cout << std::endl;
			std::cout << "]" << std::endl;
		}
		++fmt;
		if (*fmt != '\0')
			std::cout << "," << std::endl;
	}
	std::cout << "}" << std::endl;
	if ((astname != NULL) && (astname[0] != '\0'))
		std::cout << "}" << std::endl;
	va_end(args);
}

bool isInt(std::string type) {
    // Returns true if the type string type is a pointer
    // to something or an array

    if(type.substr(0, 3) == "int")
        return true;
    
    int n = type.length();

    for(int i = 0; i < n; i++) {
        if(type[i] == '*' || type[i] == '[')
            return true;
    }

    return false;
}

bool isArray(std::string type) {
	// Returns true if type string is array

	if(type.find(')') != std::string::npos) return false;
	if(type.find('[') != std::string::npos) return true;
	return false;
}

bool inCompatible(std::string t1, std::string t2, std::string tex1, std::string tex2) {
	// Get the base types

	if(t1 == t2) {
		return false;
	}

	std::string bt1, bt2;
	std::string suff1, suff2;
	int i;
	for(i = 0; i < (int) t1.length(); i++) {
		if(t1[i] == '(' || t1[i] == '*' || t1[i] == '[') {
			break;
		}
	}

	bt1 = t1.substr(0, i);
	suff1 = t1.substr(i, (int) t1.length() - i);

	for(i = 0; i < (int) t2.length(); i++) {
		if(t2[i] == '(' || t2[i] == '*' || t2[i] == '[') {
			break;
		}
	}

	bt2 = t2.substr(0, i);
	suff2 = t2.substr(i, (int) t2.length() - i);

	// Check if the pointer and array things are valid

	int c1 = 0, c2 = 0;

	for(int i = 0; i < (int) suff1.length(); i++) {
		if(suff1[i] == '*' || suff1[i] == '[')
			c1++;
	}

	for(int i = 0; i < (int) suff2.length(); i++) {
		if(suff2[i] == '*' || suff2[i] == '[')
			c2++;
	}

	if(c1 >= 1 && tex2 == "0")
		return false;

	if(bt1 == "void" && c1 >= 1) {
		if(c2 >= 1) return false;
		else return true;
	}
	else if(bt1 != bt2) {
		if(c1 >= 1 && bt2 != "void")
			return true;
		else
			return false;
	}
	else if(c1 == c2) {
		return false;
	}
	else {
		return true;
	}
}

std::string baseType(std::string type) {
	int i = 0;
	int len = type.length();

	while(i < len) {
		if(type[i] == '*' || type[i] == '(' || type[i] == '[')
			break;
		i++;
	}

	return type.substr(0, i);
}

int depth(std::string type) {
	int d = 0, i = 0;
	int len = type.length();

	while(i < len) {
		if(type[i] == '*' || type[i] == '[')
			d++;
		i++;
	}

	return d;
}

bool isPointer(std::string type) {
	int len = type.length();

	for(int i = 0; i < len; i++) {
		if(type[i] == '(') return true;

		if(type[i] == '[') return false;
	}

	if(type.find('*') != std::string::npos) return true;
	return false;
}

std::string modType(std::string type) {
	if(depth(type) == 0)
		return type;
	else if(isArray(type)) {
		int open_ind = type.find('[');
		int close_ind = type.find(']', open_ind);
		
		std::string output = type.substr(0, open_ind);
		output += "(*)";
		output += type.substr(close_ind + 1);

		if(output[output.length() - 1] == ')') {
			output = type.substr(0, open_ind) + "*";
		}

		return output;
	}
	else {
		return type;
	}
}

int typeSize(std::string type) {
	// Case one, not an array or pointer, return base type size
	// Case two, array or pointer
	// Case three, address to something
	
	if(!isArray(type) && !isPointer(type)) {
		if(type.substr(0, 6) == "struct") return gst->tab[type]->size;
		else return 4;
	}
	else if(isArray(type)) {
		// This can be an array of pointers
		std::string bt = baseType(type);

		return typeSize(modType(type));
	}
	else {
		// Surely a pointer

		if(type.find("(") != std::string::npos) {
			// Pointer to an array
			int multiplier = 1;
			int offset = 0;

			while(!(offset == (int) type.length() - 1 || type.find("[", offset) == std::string::npos)) {
				int s_ind = type.find("[", offset);
				int o_ind = type.find("]", s_ind);
				offset = o_ind;

				int num = std::stoi(type.substr(s_ind+1, (o_ind - 1) - s_ind));
				multiplier *= num;
			}

			return multiplier * typeSize(type.substr(0, type.find("(")));
		}
		else {
			// Simply a pointer
			if(depth(type) == 1) {
				if(baseType(type).substr(0, 6) != "struct") return 4;
				else return gst->tab[baseType(type)]->size;
			}
			else return 4;
		}
	}
}

void gen(std::string arg, ...) {
	code += arg + "\n";
}

void opGen(std::string arg1, std::string arg2, std::string op) {
	if(op == "PLUS") {
		gen("\taddl\t" + arg2 + ", " + arg1);
	}
	else if(op == "MINUS") {
		gen("\tsubl\t" + arg2 + ", " + arg1);
	}
	else if(op == "MULT") {
		gen("\timul\t" + arg2 + ", " + arg1);
	}
	else if(op == "DIV") {
		// idivl	oper
		// The above instruction divides edx:eax by oper
		// Places the quotient in eax and the remainder in edx
		
		// I would just store eax and edx on the stack
		// carry out the division and then reload them
		// won't care if they are being used or not

		gen("\tpushl\t%eax");
		gen("\tpushl\t%edx");		
		gen("\tpushl\t%ecx");
		if(arg1 != "%eax")
			gen("\tmovl\t" + arg1 + ", %eax");

		gen("\tcltd");
		if(isReg(arg2) && arg2 != "%eax" && arg2 != "%edx")
			gen("\tidivl\t" + arg2);
		else {
			// Here, if arg2 was %eax or %edx, I will first have
			// to obtain its value from the stack if it was pushed
			if(arg2 == "%eax")
				gen("\tmovl\t8(%esp), %ecx");
			else if(arg2 == "%edx")
				gen("\tmovl\t4(%esp), %ecx");
			else
				gen("\tmovl\t" + arg2 + ", %ecx");
			gen("\tidivl\t%ecx");
		}
		
		// Load ecx, eax and edx again
		gen("\tmovl\t%eax, " + arg1);
		
		if(!isFree("%ecx") && arg1 != "%ecx")
			gen("\tmovl\t(%esp), %ecx");
		gen("\taddl\t$4, %esp");
		
		if(!isFree("%edx")  && arg1 != "%edx")
			gen("\tmovl\t(%esp), %edx");
		gen("\taddl\t$4, %esp");

		if(!isFree("%eax") && arg1 != "%eax")
			gen("\tmovl\t(%esp), %eax");
		gen("\taddl\t$4, %esp");
	}
	else if(op == "OR_OP") {
		// Devised my own fallthrough logic
		// B = B1 || B2
		// Compare B1 and B2 for truth
		// If yes, jump to common true code label
		// At last, execute the false code and jump over true code
		// Create a new label for succeeding code too

		int trueLabel = cntLabl++;
		int commonLabel = cntLabl++;
		gen("\tcmpl\t$0, " + arg1);
		gen("\tjne\t.L" + std::to_string(trueLabel));
		gen("\tcmpl\t$0, " + arg2);
		gen("\tjne\t.L" + std::to_string(trueLabel));
		
		// Now, comes the false code
		// that is assign the value of 0 to arg1
		gen("\tmovl\t$0, " + arg1);
		gen("\tjmp\t.L" + std::to_string(commonLabel));
		
		// Here is the true code
		gen(".L" + std::to_string(trueLabel) + ":");
		gen("\tmovl\t$1, " + arg1);

		// Here is the common code
		gen(".L" + std::to_string(commonLabel) + ":");
		// This statement has to follow inspite of anything
	}
	else if(op == "AND_OP") {
		// Devised my own fallthrough logic
		// B = B1 && B2
		// Compare B1 and B2 for fallacy
		// If yes, jump to common false code label
		// At last, execute the true code and jump over false code
		// Create a new label for succeeding code too

		int falseLabel = cntLabl++;
		int commonLabel = cntLabl++;
		gen("\tcmpl\t$0, " + arg1);
		gen("\tje\t.L" + std::to_string(falseLabel));
		gen("\tcmpl\t$0, " + arg2);
		gen("\tje\t.L" + std::to_string(falseLabel));
		
		// Now, comes the true code
		// that is assign the value of 1 to arg1
		gen("\tmovl\t$1, " + arg1);
		gen("\tjmp\t.L" + std::to_string(commonLabel));
		
		// Here is the false code
		gen(".L" + std::to_string(falseLabel) + ":");
		gen("\tmovl\t$0, " + arg1);

		// Here is the common code
		gen(".L" + std::to_string(commonLabel) + ":");
		// This statement has to follow inspite of anything
	}
	else if(op == "EQ_OP") {
		// Three main instructions in all binary operators
		// Compare, Set, Movz

		gen("\tcmpl\t" + arg2 + ", " + arg1);
		std::string top_r;
		
		if(isReg(arg1)) top_r = arg1;
		else top_r = rstack.top();

		std::string low_r = getL(top_r);
		gen("\tsete\t" + low_r);
		gen("\tmovzbl\t" + low_r + ", " + top_r);
	}
	else if(op == "NE_OP") {
		// Three main instructions in all binary operators
		// Compare, Set, Movz

		gen("\tcmpl\t" + arg2 + ", " + arg1);
		std::string top_r;
		
		if(isReg(arg1)) top_r = arg1;
		else top_r = rstack.top();

		std::string low_r = getL(top_r);
		gen("\tsetne\t" + low_r);
		gen("\tmovzbl\t" + low_r + ", " + top_r);
	}
	else if(op == "LT_OP") {
		// Three main instructions in all binary operators
		// Compare, Set, Movz

		gen("\tcmpl\t" + arg2 + ", " + arg1);
		std::string top_r;
		
		if(isReg(arg1)) top_r = arg1;
		else top_r = rstack.top();

		std::string low_r = getL(top_r);
		gen("\tsetl\t" + low_r);
		gen("\tmovzbl\t" + low_r + ", " + top_r);
	}
	else if(op == "GT_OP") {
		// Three main instructions in all binary operators
		// Compare, Set, Movz

		gen("\tcmpl\t" + arg2 + ", " + arg1);
		std::string top_r;
		
		if(isReg(arg1)) top_r = arg1;
		else top_r = rstack.top();

		std::string low_r = getL(top_r);
		gen("\tsetg\t" + low_r);
		gen("\tmovzbl\t" + low_r + ", " + top_r);
	}
	else if(op == "LE_OP") {
		// Three main instructions in all binary operators
		// Compare, Set, Movz

		gen("\tcmpl\t" + arg2 + ", " + arg1);
		std::string top_r;
		
		if(isReg(arg1)) top_r = arg1;
		else top_r = rstack.top();

		std::string low_r = getL(top_r);
		gen("\tsetle\t" + low_r);
		gen("\tmovzbl\t" + low_r + ", " + top_r);
	}
	else if(op == "GE_OP") {
		// Three main instructions in all binary operators
		// Compare, Set, Movz

		gen("\tcmpl\t" + arg2 + ", " + arg1);
		std::string top_r;
		
		if(isReg(arg1)) top_r = arg1;
		else top_r = rstack.top();

		std::string low_r = getL(top_r);
		gen("\tsetge\t" + low_r);
		gen("\tmovzbl\t" + low_r + ", " + top_r);
	}
}

void getAddr(exp_astnode* obj) {
	// It is a misnomer for getLVal()

	// This function is similar to genCode and calculates the address
	if(obj->astnode_type == op_unary_astN) {
		// Check if the operator is "DEREF"
		// Return the value of the child expression
		// if it is an address

		op_unary_astnode* exp = (op_unary_astnode*) obj;
		if(*(exp->op) == "DEREF") {
			// Assuming no syntax errors
			// This just leaves the value of the expression
			genCode(exp->exp);
		}
		else if(*(exp->op) == "ADDRESS") {
			// Assuming no syntax errors
			// Find the address of the expression
			getAddr(exp->exp);
		}
	}
	else if(obj->astnode_type == member_astN) {
		// Quite similar to other operators
		// get address of the left expression
		// the left expression should be a struct
		// find the offset of the identifier from the struct

		// Assume the base case for the moment
		member_astnode* exp = (member_astnode*) obj;
		getAddr(exp->exp);

		// Find the offset from the table
		int offset = exp->id->st->tab[*(exp->id->name)]->offset;
		gen("\taddl\t$" + std::to_string(offset) + ", " + rstack.top());
	}
	else if(obj->astnode_type == arrow_astN) {
		// Quite similar to other operators
		// get address of the left expression
		// the left expression should be a struct*
		// find the offset of the identifier from the struct

		// Assume the base case for the moment
		member_astnode* exp = (member_astnode*) obj;
		genCode(exp->exp);

		// Find the offset from the table
		int offset = exp->id->st->tab[*(exp->id->name)]->offset;
		gen("\taddl\t$" + std::to_string(offset) + ", " + rstack.top());
	}
	else if(obj->astnode_type == identifier_astN) {
		// If getAddr() is getting called on identifier
		// this means that we need to get it from the lst
		// In other cases, we need to get it from the related st

		// Use the symbol table to find the offset
		// and return the address

		SymTab* lst = tables.top();
		std::string id = *(((identifier_astnode*) obj)->name);
		int offset = lst->tab[id]->offset;
		std::string addr = std::to_string(offset) + "(%ebp)";

		gen("\tleal\t" + addr + ", " + rstack.top());
	}
	else if(obj->astnode_type == arrayref_astN) {
		// get the address of exp1
		// get the value of exp2
		// go to the offset, dereference it

		// Need to do Sethi Ulman here

		arrayref_astnode* exp = (arrayref_astnode*) obj;
		int l1 = L(exp->lExp), l2 = L(exp->rExp);

		int jumpSize = typeSize(modType(exp->lExp->type));

		// if(exp->lExp->astnode_type == identifier_astN) {
		// 	identifier_astnode* id = (identifier_astnode*) (exp->lExp);
		// 	std::string* name = id->name;

		// 	if(isPointer(exp->lExp->type)) {
		// 		genCode(exp->lExp);
		// 	}
		// 	else if(isArray(exp->lExp->type) && id->st->tab[*name]->param_local == "param") {
		// 		genCode(exp->lExp);
		// 	}
		// 	else {
		// 		getAddr(exp->lExp);
		// 	}
		// }
		// else {
		// 	getAddr(exp->lExp);
		// }

		if(isLeaf(exp->rExp)) {
			if(exp->lExp->astnode_type == identifier_astN) {
				identifier_astnode* id = (identifier_astnode*) (exp->lExp);
				std::string* name = id->name;

				if(isPointer(exp->lExp->type)) {
					genCode(exp->lExp);
				}
				else if(isArray(exp->lExp->type) && id->st->tab[*name]->param_local == "param") {
					genCode(exp->lExp);
				}
				else {
					getAddr(exp->lExp);
				}
			}
			else {
				getAddr(exp->lExp);
			}
			std::string top_r = rstack.top();
			
			// Since strings won't be assigned
			// It is just the integers or identifiers

			if(exp->rExp->astnode_type == intconst_astN) {
				std::string num = *(((intconst_astnode*) exp->rExp)->num);
				// generate the appropriate code according to the operator				
				int n = std::stoi(num);

				gen("\taddl\t$" + std::to_string(n*jumpSize) + ", " + rstack.top());
			}
			else if(exp->rExp->astnode_type == identifier_astN) {
				// use the location of the identifier instead of num
				
				SymTab* lst = tables.top();
				std::string id = *(((identifier_astnode*) exp->rExp)->name);
				int offset = lst->tab[id]->offset;
				std::string addr = std::to_string(offset) + "(%ebp)";
				std::string top_r = rstack.top();
				rstack.pop();

				gen("\tmovl\t" + addr + ", " + rstack.top());
				gen("\timul\t$" + std::to_string(jumpSize) + ", " + rstack.top());
				gen("\taddl\t" + rstack.top() + ", " + top_r);
				rstack.push(top_r);
			}
		}
		else if(l1 < l2 && l1 < 6) {
			swap(rstack);
			genCode(exp->rExp);
			std::string R = rstack.top();
			rstack.pop();
			if(exp->lExp->astnode_type == identifier_astN) {
				identifier_astnode* id = (identifier_astnode*) (exp->lExp);
				std::string* name = id->name;

				if(isPointer(exp->lExp->type)) {
					genCode(exp->lExp);
				}
				else if(isArray(exp->lExp->type) && id->st->tab[*name]->param_local == "param") {
					genCode(exp->lExp);
				}
				else {
					getAddr(exp->lExp);
				}
			}
			else {
				getAddr(exp->lExp);
			}
			
			std::string top_r = rstack.top();
			// gen("\tmovl\t" + R + ", " + top_r);
			// opGen(top_r, R, *(exp->op));
			gen("\timul\t$" + std::to_string(jumpSize) + ", " + R);
			gen("\taddl\t" + R + ", " + top_r);
			rstack.push(R);
			swap(rstack);
		}
		else if(l2 <= l1 && l2 < 6) {
			if(exp->lExp->astnode_type == identifier_astN) {
				identifier_astnode* id = (identifier_astnode*) (exp->lExp);
				std::string* name = id->name;

				if(isPointer(exp->lExp->type)) {
					genCode(exp->lExp);
				}
				else if(isArray(exp->lExp->type) && id->st->tab[*name]->param_local == "param") {
					genCode(exp->lExp);
				}
				else {
					getAddr(exp->lExp);
				}
			}
			else {
				getAddr(exp->lExp);
			}
			std::string R = rstack.top();
			rstack.pop();
			genCode(exp->rExp);
			// gen("\tmovl\t" + rstack.top() + ", " + R);
			gen("\timul\t$" + std::to_string(jumpSize) + ", " + rstack.top());
			gen("\taddl\t" + rstack.top() + ", " + R);
			rstack.push(R);
		}
		else if(l1 >= 6 && l2 >= 6) {
			genCode(exp->rExp);

			gen("\timul\t$" + std::to_string(jumpSize) + ", " + rstack.top());
			gen("\tpushl\t" + rstack.top());

			if(exp->lExp->astnode_type == identifier_astN) {
				identifier_astnode* id = (identifier_astnode*) (exp->lExp);
				std::string* name = id->name;

				if(isPointer(exp->lExp->type)) {
					genCode(exp->lExp);
				}
				else if(isArray(exp->lExp->type) && id->st->tab[*name]->param_local == "param") {
					genCode(exp->lExp);
				}
				else {
					getAddr(exp->lExp);
				}
			}
			else {
				getAddr(exp->lExp);
			}

			gen("\taddl\t(%esp), " + rstack.top());
			gen("\taddl\t$4, %esp");
		}
	}
	else {
		// No addresses can be returned

		// To make sense out of the fact that we can return structs
		// this function has to be extended for funcall_astnode

		if(obj->astnode_type == funcall_astN) {
			// Whenever the return type is struct, we will use the stack
			// for communication between the two

			funcall_astnode* exp = (funcall_astnode*) obj;
			std::vector<exp_astnode*> callExps = exp->callExps;

			std::string fname = *(exp->fname->name);

			// Obtained the offsets and types of all of the parameters
			std::vector<std::pair<int, std::string>> params;
			struct SymTab st = *(gst->tab[fname]->symtab);

			for(auto it = st.tab.begin(); it != st.tab.end(); ++it) {
				stRecord entry = *(it->second);
				if(entry.offset > 0)
					params.push_back({entry.offset, entry.type});
			}

			sort(params.begin(), params.end(), std::greater<std::pair<int, std::string>>());

			// Save all the registers to the stack
			gen("\tpushl\t%eax");
			gen("\tpushl\t%ebx");
			gen("\tpushl\t%ecx");
			gen("\tpushl\t%edx");
			gen("\tpushl\t%esi");
			gen("\tpushl\t%edi");

			// Now, if the return type of the function is struct
			// then I will have to allocate space for it return
			std::string ret_type = gst->tab[fname]->type;
			if(ret_type.substr(0, 6) == "struct") {
				// Find the size of the struct and allocate
				int size = gst->tab[ret_type]->size;
				gen("\tsubl\t$" + std::to_string(size) + ", %esp");
			}

			int num_exps = callExps.size();
			for(int i = 0; i < num_exps; i++) {
				// generate the code for each of the expressions
				// and then immediately push the result to the stack

				std::string type = params[i].second;
				if(type.substr(0, 6) == "struct" && !isPointer(type) && !isArray(type)) {
					// Need to push all of the bytes in this case
					// but pushing will have to be done in the reverse order
					getAddr(callExps[i]);

					// Have the address of the first byte in rstack.top();
					int size = gst->tab[type]->size;

					// Assuming that size will be divisible by 4
					int iter = size/4;

					for(int j = iter-1; j >= 0; j--) {
						if(j != 0)
							gen("\tpushl\t" + std::to_string(j*4) + "(" + rstack.top() + ")");
						else
							gen("\tpushl\t(" + rstack.top() + ")");
					}
				}
				else {
					genCode(callExps[i]);
					gen("\tpushl\t" + rstack.top());
				}

			}
			gen("\tsubl\t$4, %esp");
			gen("\tcall\t" + fname);
			gen("\taddl\t$" + std::to_string((num_exps+1)*4) + ", %esp");
			if(ret_type.substr(0, 6) == "struct") {
				// Find the size of the struct and deallocate
				int size = gst->tab[ret_type]->size;
				gen("\taddl\t$" + std::to_string(size) + ", %esp");
			}
			
			// Now, need to move the return value to rstack.top()
			gen("\tmovl\t%eax, " + rstack.top());

			// Load all except the stack top
			if(rstack.top() != "%edi")
				gen("\tmovl\t(%esp), %edi");
			gen("\taddl\t$4, %esp");

			if(rstack.top() != "%esi")
				gen("\tmovl\t(%esp), %esi");
			gen("\taddl\t$4, %esp");

			if(rstack.top() != "%edx")
				gen("\tmovl\t(%esp), %edx");
			gen("\taddl\t$4, %esp");

			if(rstack.top() != "%ecx")
				gen("\tmovl\t(%esp), %ecx");
			gen("\taddl\t$4, %esp");

			if(rstack.top() != "%ebx")
				gen("\tmovl\t(%esp), %ebx");
			gen("\taddl\t$4, %esp");

			if(rstack.top() != "%eax")
				gen("\tmovl\t(%esp), %eax");
			gen("\taddl\t$4, %esp");
		}
	}
}

void genCode(abstract_astnode* obj) {
	if(obj->astnode_type == proccall_astN) {
		// Push the callExps to the stack and call the function
		// The only thing to worry is saving registers here
		// Rest is fine
		proccall_astnode* proc = (proccall_astnode *) obj;
		int num_exps = proc->callExps.size();

		std::string name = *(proc->fname->name);

		// Save all the registers here
		gen("\tpushl\t%eax");
		gen("\tpushl\t%ebx");
		gen("\tpushl\t%ecx");
		gen("\tpushl\t%edx");
		gen("\tpushl\t%esi");
		gen("\tpushl\t%edi");
		
		if(name == "printf") {
			for(int i = num_exps-1; i >= 0; i--) {
				if(isArray(proc->callExps[i]->type))
					getAddr(proc->callExps[i]);
				else
					genCode(proc->callExps[i]);

				// Push something to the stack here
				// Depending upon the type of the expression
				// rest of the things will happen
				// Type had to be checked below as well as here

				if(proc->callExps[i]->astnode_type == stringconst_astN) {
					gen("\tpushl\t$.LC" + std::to_string(cntLocl-1));
				}
				else {
					gen("\tpushl\t" + rstack.top());
				}
			}

			// Pushing dummy things according to assgn2 conventions
			gen("\tcall\t" + name);
			gen("\taddl\t$" + std::to_string(num_exps*4) + ", %esp");
		}
		else {
			// Obtained the offsets and types of all of the parameters
			std::vector<std::pair<int, std::string>> params;
			struct SymTab st = *(gst->tab[name]->symtab);

			for(auto it = st.tab.begin(); it != st.tab.end(); ++it) {
				stRecord entry = *(it->second);
				if(entry.offset > 0)
					params.push_back({entry.offset, entry.type});
			}

			sort(params.begin(), params.end(), std::greater<std::pair<int, std::string>>());

			// Now, if the return type of the function is struct
			// then I will have to allocate space for it return
			std::string ret_type = gst->tab[name]->type;
			if(ret_type.substr(0, 6) == "struct") {
				// Find the size of the struct and allocate
				int size = gst->tab[ret_type]->size;
				gen("\tsubl\t$" + std::to_string(size) + ", %esp");
			}

			for(int i = 0; i < num_exps; i++) {
				// generate the code for each of the expressions
				// and then immediately push the result to the stack

				std::string type = params[i].second;
				if(type.substr(0, 6) == "struct" && !isPointer(type) && !isArray(type)) {
					// Need to push all of the bytes in this case
					// but pushing will have to be done in the reverse order
					getAddr(proc->callExps[i]);

					// Have the address of the first byte in rstack.top();
					int size = gst->tab[type]->size;

					// Assuming that size will be divisible by 4
					int iter = size/4;

					for(int j = iter-1; j >= 0; j--) {
						if(j != 0)
							gen("\tpushl\t" + std::to_string(j*4) + "(" + rstack.top() + ")");
						else
							gen("\tpushl\t(" + rstack.top() + ")");
					}
				}
				else {
					if(isArray(proc->callExps[i]->type))
						getAddr(proc->callExps[i]);
					else
						genCode(proc->callExps[i]);
					gen("\tpushl\t" + rstack.top());
				}
			}
			// Pushing dummy things according to assgn2 conventions
			gen("\tsubl\t$4, %esp");
			gen("\tcall\t" + name);
			gen("\taddl\t$" + std::to_string((num_exps+1)*4) + ", %esp");
			if(ret_type.substr(0, 6) == "struct") {
				// Find the size of the struct and deallocate
				int size = gst->tab[ret_type]->size;
				gen("\taddl\t$" + std::to_string(size) + ", %esp");
			}
		}

		if(rstack.top() != "%edi")
			gen("\tmovl\t(%esp), %edi");
		gen("\taddl\t$4, %esp");

		if(rstack.top() != "%esi")
			gen("\tmovl\t(%esp), %esi");
		gen("\taddl\t$4, %esp");

		if(rstack.top() != "%edx")
			gen("\tmovl\t(%esp), %edx");
		gen("\taddl\t$4, %esp");

		if(rstack.top() != "%ecx")
			gen("\tmovl\t(%esp), %ecx");
		gen("\taddl\t$4, %esp");

		if(rstack.top() != "%ebx")
			gen("\tmovl\t(%esp), %ebx");
		gen("\taddl\t$4, %esp");

		if(rstack.top() != "%eax")
			gen("\tmovl\t(%esp), %eax");
		gen("\taddl\t$4, %esp");
	}
	else if(obj->astnode_type == stringconst_astN) {
		string_astnode* str = (string_astnode *) obj;

		locals += ".LC" + std::to_string(cntLocl++) + ":\n";
		locals += "\t.string\t" + *(str->str) + "\n";
	}
	else if(obj->astnode_type == assignE_astN || obj->astnode_type == assignS_astN) {
		// Do Sethi Ulman here
		// assigE is also one of the valid operators

		assignE_astnode* exp = (assignE_astnode*) obj;
		int l1 = L(exp->lExp), l2 = L(exp->rExp);

		exp_astnode *lExp = (exp_astnode*)(exp->lExp);
		bool isStruct = false;
		if(lExp->type.substr(0, 6) == "struct" && !isPointer(lExp->type) && !isArray(lExp->type)) isStruct = true;

		if(isLeaf(exp->rExp)) {
			getAddr(exp->lExp);
			std::string top_r = rstack.top();
			
			// Since strings won't be assigned
			// It is just the integers

			if(exp->rExp->astnode_type == intconst_astN) {
				std::string num = *(((intconst_astnode*) exp->rExp)->num);
				// generate the appropriate code according to the operator				
				// gen("\tmovl\t$" + num + ", " + top_r);

				if(!isStruct)
					gen("\tmovl\t$" + num + ", (" + top_r + ")");
			}
			else if(exp->rExp->astnode_type == identifier_astN) {
				// use the location of the identifier instead of num
				
				SymTab* lst = tables.top();
				std::string id = *(((identifier_astnode*) exp->rExp)->name);
				int offset = lst->tab[id]->offset;
				
				if(!isStruct) {
					std::string addr = std::to_string(offset) + "(%ebp)";
					rstack.pop();
					std::string R = rstack.top();
					gen("\tmovl\t" + addr + ", " + R);
					gen("\tmovl\t" + R + ", (" + top_r + ")");
					rstack.push(top_r);
				}
				else {
					// The RHS is an instance of a struct that I need to copy

					// gen("\tmovl\t" + R + ", (" + top_r + ")");
					// Have addresses of both LHS and RHS here

					int size = gst->tab[lExp->type]->size;

					// Assuming that size will be divisible by 4
					int iter = size/4;

					// top_r has the dest address
					rstack.pop();
					std::string R = rstack.top();
					for(int j = 0; j < iter; j++) {
						if(j*4 + offset != 0) {
							gen("\tmovl\t" + std::to_string(j*4 + offset) + "(%ebp), " + R);
						}
						else {
							gen("\tmovl\t(%ebp), " + R);
						}
						gen("\tmovl\t" + R + ", (" + top_r + ")");
					}
					rstack.push(top_r);
				}
			}
		}
		else if(l1 < l2 && l1 < 6) {
			if(!isStruct) {
				swap(rstack);
				genCode(exp->rExp);
				std::string R = rstack.top();
				rstack.pop();
				getAddr(exp->lExp);
				
				std::string top_r = rstack.top();
				gen("\tmovl\t" + R + ", (" + top_r + ")");
				rstack.push(R);
				swap(rstack);
			}
			else {
				swap(rstack);
				getAddr(exp->rExp);
				std::string R = rstack.top();
				rstack.pop();
				getAddr(exp->lExp);
				
				std::string top_r = rstack.top();
				// gen("\tmovl\t" + R + ", (" + top_r + ")");
				int size = gst->tab[lExp->type]->size;

				// Assuming that size will be divisible by 4
				int iter = size/4;

				std::string reg = "%eax";
				if(R != reg && reg != top_r) {}
				else {
					if(R != "%ebx" && top_r != "%ebx") {
						reg = "%ebx";
					}
					else {
						reg = "%ecx";
					}
				}
				// Saving reg
				gen("\tpushl\t" + reg);

				// Transferring the data
				for(int j = 0; j < iter; j++) {
					if(j != 0) {
						gen("\tmovl\t" + std::to_string(j*4) + "(" + R + "), " + reg);
						gen("\tmovl\t" + reg + ", " + std::to_string(j*4) + "(" + top_r + ")");
					}
					else {
						gen("\tmovl\t(" + R + "), " + reg);
						gen("\tmovl\t" + reg + ", (" + top_r + ")");
					}
				}

				// Reload reg
				gen("\tmovl\t(%esp), " + reg);
				gen("\taddl\t$4, %esp");

				rstack.push(R);
				swap(rstack);
			}
		}
		else if(l2 <= l1 && l2 < 6) {
			if(!isStruct) {
				getAddr(exp->lExp);
				std::string R = rstack.top();
				rstack.pop();
				genCode(exp->rExp);
				gen("\tmovl\t" + rstack.top() + ", (" + R + ")");
				rstack.push(R);
			}
			else {
				getAddr(exp->lExp);
				gen("\tpushl\t" + rstack.top());

				getAddr(exp->rExp);
				// R has rExp
				std::string R = rstack.top();
				rstack.pop();

				// top_r has lExp
				std::string top_r = rstack.top();
				gen("\tmovl\t(%esp), " + top_r);
				gen("\taddl\t$4, %esp");

				rstack.pop();
				std::string reg = rstack.top();

				// Data movement
				int size = gst->tab[lExp->type]->size;

				// Assuming that size will be divisible by 4
				int iter = size/4;

				// Transferring the data
				for(int j = 0; j < iter; j++) {
					if(j != 0) {
						gen("\tmovl\t" + std::to_string(j*4) + "(" + R + "), " + reg);
						gen("\tmovl\t" + reg + ", " + std::to_string(j*4) + "(" + top_r + ")");
					}
					else {
						gen("\tmovl\t(" + R + "), " + reg);
						gen("\tmovl\t" + reg + ", (" + top_r + ")");
					}
				}
				// Data movement

				rstack.push(top_r);
				rstack.push(R);
			}
		}
		else if(l1 >= 6 && l2 >= 6) {

			if(!isStruct) {
				genCode(exp->rExp);

				gen("\tpushl\t" + rstack.top());
				getAddr(exp->lExp);

				// Move the data
				// Save a reg and use it
				std::string reg = "%eax";
				if(reg == rstack.top()) reg = "%ebx";

				gen("\tpushl\t" + reg);

				gen("\tmovl\t4(%esp), " + reg);
				gen("\tmovl\t" + reg + ", (" + rstack.top() + ")");

				gen("\tmovl\t(%esp), " + reg);
				gen("\taddl\t$8, %esp");
			}
			else {
				getAddr(exp->lExp);
				gen("\tpushl\t" + rstack.top());

				getAddr(exp->rExp);
				// R has rExp
				std::string R = rstack.top();
				rstack.pop();

				// top_r has lExp
				std::string top_r = rstack.top();
				gen("\tmovl\t(%esp), " + top_r);
				gen("\taddl\t$4, %esp");

				rstack.pop();
				std::string reg = rstack.top();

				// Data movement
				int size = gst->tab[lExp->type]->size;

				// Assuming that size will be divisible by 4
				int iter = size/4;

				// Transferring the data
				for(int j = 0; j < iter; j++) {
					if(j != 0) {
						gen("\tmovl\t" + std::to_string(j*4) + "(" + R + "), " + reg);
						gen("\tmovl\t" + reg + ", " + std::to_string(j*4) + "(" + top_r + ")");
					}
					else {
						gen("\tmovl\t(" + R + "), " + reg);
						gen("\tmovl\t" + reg + ", (" + top_r + ")");
					}
				}
				// Data movement

				rstack.push(top_r);
				rstack.push(R);
			}
		}
	}
	else if(obj->astnode_type == op_binary_astN) {
		// These behave differently for pointers
		op_binary_astnode* exp = (op_binary_astnode*) obj;
		int l1 = L(exp->lExp), l2 = L(exp->rExp);

		if(isLeaf(exp->rExp)) {
			// std::cout << "Case 1" << std::endl;
			genCode(exp->lExp);
			std::string top_r = rstack.top();
			
			// Since strings won't be assigned
			// It is just the integers or identifiers

			if(exp->rExp->astnode_type == intconst_astN) {
				std::string num = *(((intconst_astnode*) exp->rExp)->num);
				// generate the appropriate code according to the operator				
				// gen("\tmovl\t$" + num + ", " + top_r);

				if(isPointer(exp->lExp->type) || isArray(exp->lExp->type)) {
					int ts = typeSize(exp->lExp->type);
					// std::cout << "ts : " << ts << " of t : " << exp->lExp->type << std::endl;
					num = std::to_string(ts*std::stoi(num));
				}
				opGen(top_r, "$"+num, *(exp->op));
			}
			else if(exp->rExp->astnode_type == identifier_astN) {
				// use the location of the identifier instead of num
				
				SymTab* lst = tables.top();
				std::string id = *(((identifier_astnode*) exp->rExp)->name);
				int offset = lst->tab[id]->offset;
				std::string addr = std::to_string(offset) + "(%ebp)";
				
				opGen(top_r, addr, *(exp->op));
			}
		}
		else if(l1 < l2 && l1 < 6) {
			// std::cout << "Case 2" << std::endl;
			swap(rstack);
			genCode(exp->rExp);
			std::string R = rstack.top();
			rstack.pop();
			genCode(exp->lExp);
			
			std::string top_r = rstack.top();
			// gen("\tmovl\t" + R + ", " + top_r);
			opGen(top_r, R, *(exp->op));
			rstack.push(R);
			swap(rstack);
		}
		else if(l2 <= l1 && l2 < 6) {
			// std::cout << "Case 3" << std::endl;
			genCode(exp->lExp);	
			std::string R = rstack.top();
			rstack.pop();
			genCode(exp->rExp);
			// gen("\tmovl\t" + rstack.top() + ", " + R);
			opGen(R, rstack.top(), *(exp->op));
			rstack.push(R);
		}
		else if(l1 >= 6 && l2 >= 6) {
			// std::cout << "Case 4" << std::endl;
			genCode(exp->rExp);
			
			gen("\tpushl\t" + rstack.top());
			genCode(exp->lExp);

			opGen(rstack.top(), "(%esp)", *(exp->op));

			gen("\taddl\t$4, %esp");
		}
		else {
			std::cout << "Case 5" << std::endl;
		}

	}
	else if(obj->astnode_type == op_unary_astN) {
		op_unary_astnode* exp = (op_unary_astnode*) obj;
		genCode(exp->exp);
		if(*(exp->op) == "UMINUS") {
			gen("\tnegl\t" + rstack.top());
		}
		else if(*(exp->op) == "NOT") {
			// Fucking different
			// Just compare with 0
			// If 0, then answer 1 else 0
			if(!isFree("%eax"))
				gen("\tpushl\t%eax");
			
			gen("\tcmpl\t$0, " + rstack.top());
			gen("\tsete\t%al");
			gen("\tmovzbl\t%al, %eax");
			gen("\tmovl\t%eax, " + rstack.top());

			if(!isFree("%eax")) {
				gen("\tmovl\t(%esp), %eax");
				gen("\taddl\t$4, %esp");
			}
		}
		else if(*(exp->op) == "ADDRESS") {
			// Assuming that addressing is always called on variables
			// Modify the part below
			// Assume that structs won't be present here

			// Assuming that exp->exp is only identifier_astnode rn

			// std::string var = *(((identifier_astnode*) exp->exp)->name);
			// SymTab* lst = tables.top();
			// int offset = lst->tab[var]->offset;

			// gen("\tleal\t" + std::to_string(offset) + "(%ebp), " + rstack.top());

			getAddr(exp->exp);
		}
		else if(*(exp->op) == "DEREF") {
			gen("\tmovl\t(" + rstack.top() + "), " + rstack.top());
		}
		else if(*(exp->op) == "PP") {
			// Fucking different

			// Need to increment the value by 1
			gen("\tpushl\t" + rstack.top());
			getAddr(exp->exp);

			int num = 1;
			std::string type = exp->exp->type;
			if(isPointer(type) || isArray(type)) {
				num = typeSize(type);
			}

			gen("\taddl\t$" + std::to_string(num) + ", (" + rstack.top() + ")");
			gen("\tmovl\t(" + rstack.top() + "), " + rstack.top());
			gen("\tsubl\t$" + std::to_string(num) + ", " + rstack.top());
			gen("\taddl\t$4, %esp");
		}
	}
	else if(obj->astnode_type == funcall_astN) {
		// Will have to store caller and callee saved registers somewhere

		// Whenever the return type is struct, we will use the stack
		// for communication between the two

		funcall_astnode* exp = (funcall_astnode*) obj;
		std::vector<exp_astnode*> callExps = exp->callExps;

		std::string fname = *(exp->fname->name);

		// Obtained the offsets and types of all of the parameters
		std::vector<std::pair<int, std::string>> params;
		struct SymTab st = *(gst->tab[fname]->symtab);

		for(auto it = st.tab.begin(); it != st.tab.end(); ++it) {
			stRecord entry = *(it->second);
			if(entry.offset > 0)
				params.push_back({entry.offset, entry.type});
		}

		sort(params.begin(), params.end(), std::greater<std::pair<int, std::string>>());

		// Save all the registers to the stack
		gen("\tpushl\t%eax");
		gen("\tpushl\t%ebx");
		gen("\tpushl\t%ecx");
		gen("\tpushl\t%edx");
		gen("\tpushl\t%esi");
		gen("\tpushl\t%edi");

		// Now, if the return type of the function is struct
		// then I will have to allocate space for it return
		std::string ret_type = gst->tab[fname]->type;
		if(ret_type.substr(0, 6) == "struct") {
			// Find the size of the struct and allocate
			int size = gst->tab[ret_type]->size;
			gen("\tsubl\t$" + std::to_string(size) + ", %esp");
		}

		int num_exps = callExps.size();
		for(int i = 0; i < num_exps; i++) {
			// generate the code for each of the expressions
			// and then immediately push the result to the stack

			std::string type = params[i].second;
			if(type.substr(0, 6) == "struct" && !isPointer(type) && !isArray(type)) {
				// Need to push all of the bytes in this case
				// but pushing will have to be done in the reverse order
				getAddr(callExps[i]);

				// Have the address of the first byte in rstack.top();
				int size = gst->tab[type]->size;

				// Assuming that size will be divisible by 4
				int iter = size/4;

				for(int j = iter-1; j >= 0; j--) {
					if(j != 0)
						gen("\tpushl\t" + std::to_string(j*4) + "(" + rstack.top() + ")");
					else
						gen("\tpushl\t(" + rstack.top() + ")");
				}
			}
			else {
				if(isArray(callExps[i]->type))
					getAddr(callExps[i]);
				else
					genCode(callExps[i]);
				gen("\tpushl\t" + rstack.top());
			}

		}
		gen("\tsubl\t$4, %esp");
		gen("\tcall\t" + fname);
		gen("\taddl\t$" + std::to_string((num_exps+1)*4) + ", %esp");
		if(ret_type.substr(0, 6) == "struct") {
			// Find the size of the struct and deallocate
			int size = gst->tab[ret_type]->size;
			gen("\taddl\t$" + std::to_string(size) + ", %esp");
		}
		
		// Now, need to move the return value to rstack.top()
		gen("\tmovl\t%eax, " + rstack.top());

		// Load all except the stack top
		if(rstack.top() != "%edi")
			gen("\tmovl\t(%esp), %edi");
		gen("\taddl\t$4, %esp");

		if(rstack.top() != "%esi")
			gen("\tmovl\t(%esp), %esi");
		gen("\taddl\t$4, %esp");

		if(rstack.top() != "%edx")
			gen("\tmovl\t(%esp), %edx");
		gen("\taddl\t$4, %esp");

		if(rstack.top() != "%ecx")
			gen("\tmovl\t(%esp), %ecx");
		gen("\taddl\t$4, %esp");

		if(rstack.top() != "%ebx")
			gen("\tmovl\t(%esp), %ebx");
		gen("\taddl\t$4, %esp");

		if(rstack.top() != "%eax")
			gen("\tmovl\t(%esp), %eax");
		gen("\taddl\t$4, %esp");
	}
	else if(obj->astnode_type == intconst_astN) {
		// If genCode() is being called for this type of node
		// then it surely is the left leaf

		// Just need to load the value in a register

		intconst_astnode* exp = (intconst_astnode*) obj;
		std::string num = *(exp->num);

		gen("\tmovl\t$" + num + ", " + rstack.top());
	}
	else if(obj->astnode_type == stringconst_astN) {
		// These are present only in printf() calls
		// And I have handled them separately so no need
		// to do anything here
	}
	else if(obj->astnode_type == member_astN) {
		// Just get the address first, then deref it
		
		getAddr((exp_astnode*) obj);
		gen("\tmovl\t(" + rstack.top() + "), " + rstack.top());
	}
	else if(obj->astnode_type == arrow_astN) {
		// Similar to member_astnode

		getAddr((exp_astnode*) obj);
		gen("\tmovl\t(" + rstack.top() + "), " + rstack.top());
	}
	else if(obj->astnode_type == identifier_astN) {
		// Load the variable into a register

		identifier_astnode* exp = (identifier_astnode*) obj;
		SymTab* lst = tables.top();
		std::string id = *(exp->name);
		int offset = lst->tab[id]->offset;
		std::string addr = std::to_string(offset) + "(%ebp)";

		gen("\tmovl\t" + addr + ", " + rstack.top());
	}
	else if(obj->astnode_type == arrayref_astN) {
		arrayref_astnode* exp = (arrayref_astnode*) obj;
		// int l1 = L(exp->lExp), l2 = L(exp->rExp);

		// Need to get the address of the left expression
		// the right one will be an integer
		getAddr(exp);
		gen("\tmovl\t(" + rstack.top() + "), " + rstack.top());

		// // Binary Sethi Ullman here again
		// if(exp->rExp->astnode_type == intconst_astN) {
		// 	genCode(exp->lExp);
		// 	std::string top_r = rstack.top();
			
		// 	// Since strings won't be assigned
		// 	// It is just the integers or identifiers
		// 	std::string num = *(((intconst_astnode*) exp->rExp)->num);
		// 	// generate the appropriate code according to the operator
			
		// 	int n = std::stoi(num);
		// 	// The line below might be invalid in a certain case
		// 	gen("\tmovl\t" + std::to_string(n*4) + "(" + top_r + "), " + top_r);
		// }
		// else if(exp->rExp->astnode_type == identifier_astN) {
		// 	genCode(exp->lExp);
		// 	std::string top_r = rstack.top();
		// 	rstack.pop();
		// 	genCode(exp->rExp);
			
		// 	gen("\tmovl\t(" + top_r + ", " + rstack.top() + ", 4), " + top_r);
		// 	rstack.push(top_r);			
		// }
		// else if(l1 < l2 && l1 < 6) {
		// 	swap(rstack);
		// 	genCode(exp->rExp);
		// 	std::string R = rstack.top();
		// 	rstack.pop();
		// 	genCode(exp->lExp);
			
		// 	std::string top_r = rstack.top();
		// 	// gen("\tmovl\t" + R + ", " + top_r);
		// 	gen("\tmovl\t(" + top_r + ", " + R + ", 4), " + top_r);
		// 	rstack.push(R);
		// 	swap(rstack);
		// }
		// else if(l2 <= l1 && l2 < 6) {
		// 	genCode(exp->lExp);
		// 	std::string R = rstack.top();
		// 	rstack.pop();
		// 	genCode(exp->rExp);
		// 	// gen("\tmovl\t" + rstack.top() + ", " + R);
		// 	gen("\tmovl\t(" + R + ", " + rstack.top() + ", 4), " + R);
		// 	rstack.push(R);
		// }
		// else if(l1 >= 6 && l2 >= 6) {
		// 	genCode(exp->rExp);
			
		// 	// Assuming a reverse stack of temporaries
		// 	// will storing everything in reverse

		// 	std::string T = "%esp";
		// 	gen("\tsubl\t$4, %esp");
		// 	gen("\tmovl\t" + rstack.top() + ", " + T);
		// 	genCode(exp->lExp);
		// 	// gen("\tmovl\t" + T + ", " + rstack.top());
		// 	gen("\tmovl\t(" + rstack.top() + ", " + T + ", 4), " + rstack.top());
		// 	gen("\taddl\t$4, %esp");
		// }
	}
	else if(obj->astnode_type == empty_astN) {
		// Do nothing
		// I don't think that it will be ever called
		gen("\tnop");
	}
	else if(obj->astnode_type == seq_astN) {
		// Need to call genCode for each of the statements

		seq_astnode* exp = (seq_astnode*) obj;
		int num_stmts = exp->stmts.size();

		for(int i = 0; i < num_stmts; i++) {
			genCode(exp->stmts[i]);
		}
	}
	else if(obj->astnode_type == return_astN) {
		// Just evaluate the expression and then push to eax

		return_astnode* exp = (return_astnode*) obj;
		std::string ret_type = *(exp->ret_type);
		
		// Obtain the sum of sizes of all parameters
		std::string fname = *(exp->fun_name);
		int psize = 0;

		struct SymTab st = *(gst->tab[fname]->symtab);

		for(auto it = st.tab.begin(); it != st.tab.end(); ++it) {
			stRecord entry = *(it->second);
			if(entry.offset > 0)
				psize += entry.size;
		}


		if(ret_type.substr(0, 6) == "struct") {
			// Push struct to the required address

			// Need to push all of the bytes in this case
			// but pushing will have to be done in the reverse order
			getAddr(exp->retExp);

			// Have the address of the first byte in rstack.top();
			int size = gst->tab[ret_type]->size;

			// Assuming that size will be divisible by 4
			int iter = size/4;

			// Will have to store the movl address first and then carry out
			// the actual movement

			// Saving one register and using it for movement
			std::string reg = "%eax";
			if(rstack.top() == reg) reg = "%ebx";

			gen("\tpushl\t" + reg);

			for(int j = 0; j < iter; j++) {
				if(j != 0) {
					gen("\tmovl\t" + std::to_string(j*4) + "(" + rstack.top() + "), " + reg);
				}
				else {
					gen("\tmovl\t(" + rstack.top() + "), " + reg);
				}
					gen("\tmovl\t" + reg + ", " + std::to_string(j*4 + 12 + psize) + "(%ebp)");
			}

			// Loading the register back again

			gen("\tmovl\t(%esp), " + reg);
			gen("\taddl\t$4, %esp");
		}
		else {
			genCode(exp->retExp);
			gen("\tmovl\t" + rstack.top() + ", %eax");
		}
		gen("\tleave");
		gen("\tret");
	}
	else if(obj->astnode_type == if_astN) {
		// First evaluate the boolean expression into the tos
		// compare it with 0
		// Create labels and add jump statements

		if_astnode* exp = (if_astnode*) obj;
		genCode(exp->cond);
		
		// Have the value in the top of the stack
		// Just compare and move according to the labels now

		gen("\tcmpl\t$0, " + rstack.top());
		int trueLabel = cntLabl++;
		gen("\tjne\t.L" + std::to_string(trueLabel));
		// Code for false statement goes here
		genCode(exp->elseStmt);
		int contLabel = cntLabl++;
		gen("\tjmp\t.L" + std::to_string(contLabel));
		gen(".L" + std::to_string(trueLabel) + ":");
		genCode(exp->ifStmt);
		gen(".L" + std::to_string(contLabel) + ":");
	}
	else if(obj->astnode_type == while_astN) {
		// Using my own fallthrough logic
		// create a new label with nop, just for jumping back
		// gen code for the condition
		// if false jump out
		// gen code for the statement
		// jump to newlabel
		// outlabel with nop

		while_astnode* exp = (while_astnode*) obj;

		int newLabel = cntLabl++;
		int outLabel = cntLabl++;

		gen(".L" + std::to_string(newLabel) + ":");
		gen("\tnop");
		genCode(exp->cond);
		gen("\tcmpl\t$0, " + rstack.top());
		gen("\tje\t.L" + std::to_string(outLabel));
		genCode(exp->stmt);
		gen("\tjmp\t.L" + std::to_string(newLabel));
		gen(".L" + std::to_string(outLabel) + ":");
		gen("\tnop");
	}
	else if(obj->astnode_type == for_astN) {
		// Following gcc this time
		
		for_astnode* exp = (for_astnode*) obj;
		genCode(exp->init);
		int outLabel = cntLabl++;
		int inLabel = cntLabl++;
		gen("\tjmp\t.L" + std::to_string(outLabel));
		gen(".L" + std::to_string(inLabel) + ":");
		genCode(exp->stmt);
		genCode(exp->iter);
		gen(".L" + std::to_string(outLabel) + ":");
		genCode(exp->guard);
		gen("\tcmpl\t$0, " + rstack.top());
		gen("\tjne\t.L" + std::to_string(inLabel));
	}
}

bool isLeaf(abstract_astnode* obj) {
	// Will have to consider things stored in the memory as
	// leaves too
	if(obj->astnode_type == intconst_astN) return true;

	// Strings won't be used anywhere
	if(obj->astnode_type == stringconst_astN) return true;
	if(obj->astnode_type == identifier_astN) return true;
	return false;
}

int L(abstract_astnode* obj) {
	if(obj->astnode_type == op_binary_astN) {
		op_binary_astnode* exp = (op_binary_astnode*) obj;
		int l1 = L(exp->lExp), l2 = L(exp->rExp);

		if(isLeaf(exp->rExp)) l2 = 0;

		if(l1 == l2) return l1+1;
		return std::max(l1, l2);
	}
	else if(obj->astnode_type == op_unary_astN) {
		op_unary_astnode* exp = (op_unary_astnode*) obj;
		int l1 = L(exp->exp);

		if(l1 == 0) return 1;
		return l1;
	}
	else if(obj->astnode_type == assignE_astN) {
		// Doesn't get used anywhere in the logic so label doesn't matter
		return 1;
	}
	else if(obj->astnode_type == funcall_astN) {
		// Since, all of the argument expressions
		// are independent of each other, it doesn't matter if we reuse
		// all of the registers

		return 1;
	}
	else if(obj->astnode_type == intconst_astN) {
		return 1;
	}
	else if(obj->astnode_type == stringconst_astN) {
		return 1;
	}
	else if(obj->astnode_type == member_astN) {
		// Treating this as an operator

		member_astnode* exp = (member_astnode*) obj;
		int l1 = L(exp->exp), l2 = L(exp->id);

		if(l1 == l2) return l1+1;
		return std::max(l1, l2);
	}
	else if(obj->astnode_type == arrow_astN) {
		// Treating this as an operator

		arrow_astnode* exp = (arrow_astnode*) obj;
		int l1 = L(exp->exp), l2 = L(exp->id);

		if(l1 == l2) return l1+1;
		return std::max(l1, l2);
	}
	else if(obj->astnode_type == identifier_astN) {
		return 1;
	}
	else if(obj->astnode_type == arrayref_astN) {
		arrayref_astnode* exp = (arrayref_astnode*) obj;
		int l1 = L(exp->lExp), l2 = L(exp->rExp);

		if(isLeaf(exp->rExp)) l2 = 0;

		if(l1 == l2) return l1+1;
		return std::max(l1, l2);
	}
	// Will never be the case but just putting
	// the following return statement to avoid warning
	return 1;
}