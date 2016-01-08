#include "Hash2.h"

/**
 * Hash2 creates a hash table of symbols and their index for use with CSSR
 */
HashTable2::HashTable2() {
  entries = unordered_map();
}

HashTable2::~HashTable2() {
  delete entries;
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
void HashTable2::Insert(char *string, int index) {
  if (string == NULL) {
    cerr << "Cannot insert null pointer into Hash Table\n";
    exit(1);
  }

  char *tempstring = NULL;
  tempstring = new char[strlen(string) + 1];
  strcpy(tempstring, string);

  std::pair<char*, int> newEntry (tempstring, index);
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
int HashTable2::WhichIndex(char *string) {
  if ((string != NULL) && (string[0] == '\0')) {
    cerr << "Cannot check matching state for empty string\n";
    exit(1);
  }

  std::unordered_map<char*,int>::const_iterator got = entries.find(string);

  if (got == entries.end()) {
    cerr << "HashTable2::WhichIndex: String or symbol not in table.\n"
    << "A string/history has been encountered in the data which has"
    << "not been recorded in the set of states.  "
    << "See 'ReadMe' file for details";
    exit(1);
  } else {
    return got->second;
  }
}
