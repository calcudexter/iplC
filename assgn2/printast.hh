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