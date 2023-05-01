#include <fstream>
#include "scanner.hh"
#include "parser.tab.hh"
using namespace std;

extern SymTab* gst;
extern std::map<std::string, abstract_astnode*> ast;
vector<pair<string, SymTab*>> gststruct, gstfun;
extern std::string code, locals;
int main(const int argc, const char **argv)
{
  using namespace std;
  fstream in_file, out_file;

  in_file.open(argv[1], ios::in);
  // Generate a scanner
  IPL::Scanner scanner(in_file);
  // Generate a Parser, passing the scanner as an argument.
  // Remember %parse-param { Scanner  &scanner  }
  IPL::Parser parser(scanner);
  
  #ifdef YYDEBUG
   parser.set_debug_level(1);
  #endif 

  // The parser will execute all the required actions
  // and fill the global variables as I need
  parser.parse();

  // // create gstfun with function entries only
  // for (const auto &entry : gst->tab) {
  //   if(entry.second->var_func == "fun") {
  //     gstfun.push_back({entry.first, entry.second->symtab});
  //   }
  // }

  // // create gstfun with struct entries only
  // for (const auto &entry : gst->tab) {
  //   if(entry.second->var_func == "struct") {
  //     gststruct.push_back({entry.first, entry.second->symtab});
  //   }
  // }

  // // start the JSON printing
  // cout << "{\"globalST\": " << endl;
  // gst->printgst();
  // cout << "," << endl;

  // cout << "  \"structs\": [" << endl;
  // for(auto it = gststruct.begin(); it != gststruct.end(); ++it) {
  //   cout << "{" << endl;
  //   cout << "\"name\": " << "\"" << it->first << "\"," << endl;
  //   cout << "\"localST\": " << endl;
  //   it->second->print();
  //   cout << "}" << endl;
  //   if (next(it,1) != gststruct.end()) 
  //     cout << "," << endl;
  // }

  // cout << "]," << endl;
  // cout << "  \"functions\": [" << endl;

  // for(auto it = gstfun.begin(); it != gstfun.end(); ++it) {
  //   cout << "{" << endl;
  //   cout << "\"name\": " << "\"" << it->first << "\"," << endl;
  //   cout << "\"localST\": " << endl;
  //   it->second->print();
  //   cout << "," << endl;
  //   cout << "\"ast\": " << endl;
  //   ast[it->first]->print(0);
  //   cout << "}" << endl;
  //   if (next(it,1) != gstfun.end()) 
  //     cout << "," << endl;
  // }

  // cout << "]" << endl;
  // cout << "}" << endl;

  if(locals != "") {
    cout << "\t.section\t.rodata\n" << endl;
    cout << locals << endl;
  }
  cout << code << endl;

  fclose(stdout);
}
