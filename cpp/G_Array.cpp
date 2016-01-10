///////////////////////////////////////////////////////////////////////////////
// Title		:	G_Array
// Date			:	March 20, 2002
// Author		:	Kristina Klinkner
// Description	:	creates an array of pointers to elements, which
//                      can be any size that will fit into memory.
///////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
//
//    Copyright (C) 2002 Kristina Klinkner
//    This file is part of CSSR
//
//    CSSR is free software; you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation; either version 2 of the License, or
//    (at your option) any later version.
//
//    CSSR is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with CSSR; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//////////////////////////////////////////////////////////////////////////////

#include "G_Array.h"

G_Array::G_Array() {
  storageVector = std::vector<ArrayElem*>();
}

G_Array::~G_Array() {
  storageVector.clear();
  storageVector.shrink_to_fit();
};

void G_Array::Insert(char string[], int counts[], int length) {
  ArrayElem *temp = new ArrayElem;
  temp->setString(string);
  temp->setCounts(counts, length);
  storageVector.push_back(temp);
}

long G_Array::getSize() {
  return storageVector.size();
}

ArrayElem * * G_Array::getList() {
  return &storageVector[0];
}

/////////////////////////////////////////////////////////////
//Function: ArrayElem::setCounts
//Purpose: set m_counts to specified value
//In parameters: the array with values and the length of the
//               array
////////////////////////////////////////////////////////////
void ArrayElem::setCounts(int counts[], int length) {
  m_counts = new int[length];

  for (int i = 0; i < length; i++) {
    m_counts[i] = counts[i];
  }
}


/////////////////////////////////////////////////////////////
//Function: ArrayElem::setString
//Purpose: set m_string to specified value
//In parameters: the string of desired value
////////////////////////////////////////////////////////////
void ArrayElem::setString(char string[]) {
  m_string = new char[strlen(string) + 1];
  strcpy(m_string, string);
}
