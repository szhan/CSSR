/////////////////////////////////////////////////////////////////////////////
//Title:	Main.cpp (for program CSSR)
//Author:	Kristina Klinkner
//Date:		July 23, 2003
//Description:	Creates separate causal states for each history of data
//		with a singular probability distribution.  History length
//		increases incrementally until cutoff point is reached.  Then
//              removes transient states, determinizes remaining states, and
//              calculates various metrics for the resulting state machine.
//              Outputs a file of states, a file of state sequences, a dot
//              file, and an information file with the metrics.
//
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
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

/////////////////////////////////////////////////////////////////////////////
//
//Title:	RCSSR.cpp (modified from Main.cpp)
//Author:	Shing Hei Zhan
//Date:		June 28, 2017
//Description:	Parts of the original CSSR source code has been rewritten to allow
//		calling withn R console.
//
/////////////////////////////////////////////////////////////////////////////


#include "RCSSR.h"


//' Run CSSR Algorithm
//'
//' Write some docs!
//'
//' @param alphabet something
//' @param data something
//' @param maxLength something
//' @param isMulti something
//' @param chiSquare something
//' @param sigLevel something
//'
//' @export
// [[Rcpp::export]]
Rcpp::List runCSSR(	const Rcpp::CharacterVector alphabet, const Rcpp::CharacterVector data,
			const int maxLength, const bool isMulti, const bool isChi, const double sigLevel,
			const Rcpp::CharacterVector output_prefix){
  //alphabet as list of characters
  //data as list of characters
  //maximum length of string
  //boolean to indicate multiline data
  //boolean to indicate use of chi-squared test
  string alphabet_str = Rcpp::as<string>(alphabet);
  string data_str = Rcpp::as<string>(data);
  
  char *alphabet_ref = new char [alphabet_str.length() + 1];
  char *data_ref = new char [data_str.length() + 1];
  
  strcpy(alphabet_ref, alphabet_str.c_str());
  strcpy(data_ref, data_str.c_str());
  
  HashTable2 *alphaHash;
  Machine *machine;
  bool stateRemoved = false;	//dummy

  //create parse tree to store all strings in data
  ParseTree parsetree(maxLength);

  //read in data and alphabet from files
  parsetree.ReadInputByR(alphabet_ref, data_ref);
  //enter data in tree
  parsetree.FillTree();

  //make hash table of alpha symbols and indices
  alphaHash = parsetree.MakeAlphaHash();

  //create array of states
  AllStates allstates(parsetree.getAlphaSize(), sigLevel, isChi);

  //calculate frequency of occurence of symbols
  allstates.InitialFrequencies(parsetree);

  //check all possible strings up to max
  //length and compare distributions
  for (int k = 1; k <= maxLength; k++) {
    allstates.CalcNewDist(k, parsetree);
  }

  //remove shorter strings
  stateRemoved = allstates.DestroyShortHists(maxLength, parsetree);

  //remove all non-recurring states
  allstates.CheckConnComponents(parsetree);

  //check futures longer than 1,
  //by using determinism of states
  allstates.Determinize(parsetree);

  //remove all non-recurring states (again, since there may be new ones)
  allstates.CheckConnComponents(parsetree);

  //store transitions from state to state
  allstates.StoreTransitions(parsetree.getMaxLength(), parsetree.getAlpha());

  //calculate distribution/frequency of states
  string file_name = Rcpp::as<string>(output_prefix);
  char *data_file = new char [file_name.length() + 1];
  strcpy(data_file, file_name.c_str());

  allstates.GetStateDistsMulti(parsetree, data_file, alphaHash, isMulti);

  //calculate information values
  machine = new Machine(&allstates);
  machine->CalcRelEnt(parsetree, alphaHash, isMulti);
  machine->CalcRelEntRate(parsetree, alphaHash, isMulti);
  machine->CalcCmu();
  machine->CalcEntRate();
  machine->CalcVariation(parsetree, alphaHash, isMulti);

  //print out states
  allstates.PrintOut(data_file, parsetree.getAlpha());

  //print out machine and calculationsf
  Rcpp::List printOutForR = machine->PrintOutToR(maxLength, sigLevel, isMulti, isChi,
						parsetree.getAlphaSize(), parsetree.getAlpha());

  delete machine;

  return printOutForR;
}

