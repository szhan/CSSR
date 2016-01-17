#include "SymbolToIndexMap.h"

/**
 * Hash2 creates a hash table of symbols and their index for use with CSSR
 */
SymbolToIndexMap::SymbolToIndexMap() {
  entries = unordered_map<std::string, int>();
}

SymbolToIndexMap::~SymbolToIndexMap() {
}

/**
 * Inserts a new element in the hash table, if there is already an element at
 * the appropriate index, puts new element at the front of the list.
 *
 * Exits if the string is NULL.
 *
 * @param string      The string to insert into the hashtable.
 * @param index       The int of the index to insert.
 */
void SymbolToIndexMap::insert(char *string, int index) {
  if (string == NULL) {
    cerr << "Cannot insert null pointer into Hash Table\n";
    exit(1);
  }

  char *tempstring = NULL;
  tempstring = new char[strlen(string) + 1];
  strcpy(tempstring, string);

  std::string insertable(tempstring);
  std::pair<std::string, int> newEntry (insertable, index);
  entries.insert(newEntry);
}


/**
 * Checks to see which state string is in.
 *
 * Exits if the string is invalid or if the entry cannot be found.
 *
 * @param string      The string to find in the hashtable.
 * @return            The integer which points to the appropriate state.
 */
int SymbolToIndexMap::findIndex(char *string) {
  if (string == NULL) {
    cerr << "Cannot check matching state for empty string\n";
    exit(1);
  }

  std::string findable (string);
  std::unordered_map<std::string,int>::const_iterator got = entries.find(findable);

  if (got == entries.end()) {

    std::cout << "mymap contains:";
    for ( auto it = entries.begin(); it != entries.end(); ++it ) {
      std::cout << " " << it->first << ":" << it->second << endl;
      cout<< std::to_string(string == it->first) << endl;
    }
    std::cout << std::endl;

    std::string str(string);
    cerr << "SymbolToIndexMap::findIndex: String or symbol \""+str+ "\" not in table.\n"
    << "A string/history has been encountered in the data which has"
    << "not been recorded in the set of states.  "
    << "See 'ReadMe' file for details";
    exit(1);
  } else {
    return got->second;
  }
}
