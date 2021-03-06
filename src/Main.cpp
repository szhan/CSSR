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
//Description:	Parts of the original CSSR source code has been rewritten
//		to allow calling withn R console.
//
/////////////////////////////////////////////////////////////////////////////


#include "Main.h"


//' Run CSSR Algorithm
//'
//' Run CSSR algorithm to creates separate causal states for each history of data
//'		with a singular probability distribution.  History length
//'		increases incrementally until cutoff point is reached.  Then
//'             removes transient states, determinizes remaining states, and
//'             calculates various metrics for the resulting state machine.
//'
//' @param alphabet String representing alphabet
//' @param data String representing discrete-state time series data
//' @param maxLength Maximum history length
//' @param isChi Boolean to indicate whether to perform chi-square test
//' @param sigLevel Significant level
//' @param outputPrefix Prefix for output files
//'
//' @return a named list
//' @export
// [[Rcpp::export]]
Rcpp::List runCSSR(	const Rcpp::CharacterVector alphabet,
			const Rcpp::CharacterVector data,
			const int maxLength,
			const bool isChi,
			const double sigLevel,
			const Rcpp::CharacterVector outputPrefix){
  string alphabetStr = Rcpp::as<string>(alphabet);
  string dataStr = Rcpp::as<string>(data);

  char *alphabetRef = new char [alphabetStr.length() + 1];
  char *dataRef = new char [dataStr.length() + 1];

  strcpy(alphabetRef, alphabetStr.c_str());
  strcpy(dataRef, dataStr.c_str());

  bool isMulti = false;	//hidden
  HashTable2 *alphaHash;
  Machine *machine;
  bool stateRemoved = false;	//dummy

  //create parse tree to store all strings in data
  ParseTree parsetree(maxLength);

  //read in data and alphabet from files
  parsetree.ReadInputByR(alphabetRef, dataRef);
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
  string fileName = Rcpp::as<string>(outputPrefix);
  char *dataFile = new char [fileName.length() + 1];
  strcpy(dataFile, fileName.c_str());

  allstates.GetStateDistsMulti(parsetree, dataFile, alphaHash, isMulti);

  //calculate information values
  machine = new Machine(&allstates);
  machine->CalcRelEnt(parsetree, alphaHash, isMulti);
  machine->CalcRelEntRate(parsetree, alphaHash, isMulti);
  machine->CalcCmu();
  machine->CalcEntRate();
  machine->CalcVariation(parsetree, alphaHash, isMulti);

	//print out info
	int dataSize = parsetree.getDataSize();
	int adjustedDataSize = parsetree.getAdjustedDataSize();
	Rcpp::List printOutForRInfo = Rcpp::List::create(	Rcpp::_["data_size"] = dataSize,
								Rcpp::_["adj_data_size"] = adjustedDataSize);

	//print out states
	Rcpp::List printOutForRAllStates = allstates.PrintOutToR(parsetree.getAlpha());
	//allstates.PrintOut(dataFile, parsetree.getAlpha());

	//print out machine and calculationsf
	Rcpp::List printOutForRMachines = machine->PrintOutToR(maxLength, sigLevel, isChi,
								parsetree.getAlphaSize(), parsetree.getAlpha());

	//combine Rcpp lists
	Rcpp::List printOutForRCombined = Rcpp::List::create(	Rcpp::_["info"] = printOutForRInfo,
								Rcpp::_["results"] = printOutForRMachines,
								Rcpp::_["matrices"] = printOutForRAllStates);

	delete machine;

	return printOutForRCombined;
}

