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

class HashTable2 {
protected:
    std::unordered_map<char*, int> entries;

public:
    HashTable2();

    ~HashTable2();

    void Insert(char *string, int index);

    int WhichIndex(char *string);
};

#endif
