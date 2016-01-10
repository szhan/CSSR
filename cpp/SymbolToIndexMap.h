////////////////////////////////////////////////////////////////////////
//Title:	Hash2.h
//Author:	Kristina Klinkner
//Date:		March 20, 2002
//Description:	Header file for Hash2.cpp
//
////////////////////////////////////////////////////////////////////////

#ifndef  __HASH2_H
#define  __HASH2_H
#define HASHSIZE2  19

#include <unordered_map>
#include "Common.h"

class SymbolToIndexMap {
protected:
    std::unordered_map<std::string, int> entries;

public:
    SymbolToIndexMap();

    ~SymbolToIndexMap();

    void insert(char *string, int index);

    int findIndex(char *string);
};

#endif
