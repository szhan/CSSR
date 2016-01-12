////////////////////////////////////////////////////////////////////////
// Title:        Machine.h
// Author:       Kristina Klinkner
// Date:         March 20, 2002
// Description:  Header file for Machine.cpp
/////////////////////////////////////////////////////////////////////////////
#ifndef __MACHINE_H
#define __MACHINE_H

#include "States.h"
#include "AllStates.h"
#include "Common.h"
#include "G_Array.h"
#include "SymbolToIndexMap.h"
#include "Hash.h"

class Machine {
public:
    Machine(AllStates *allstates);

    ~Machine() { };

    void CalcStringProbs(
        G_Array *g_array,
        int maxLength,
        SymbolToIndexMap *hashtable,
        double stringProbs[]);

    double CalcStringProb(char *string, SymbolToIndexMap *alphabetMap);

    void CalcRelEnt(ParseTree &parsetree, SymbolToIndexMap *hashtable, bool isMulti);

    double CalcRelEntRateHist(
        double *stringProbs,
        ArrayElem **list,
        SymbolToIndexMap *hashtable,
        int index,
        char *alpha,
        int alphaSize,
        int adjustedDataSize);

    double CalcRelEntRateAlpha(
        double stringProb,
        char *history,
        double &accumulatedInferredRatio,
        double dataDist,
        char alphaElem,
        SymbolToIndexMap *hashtable);

    void CalcRelEntRate(ParseTree &parsetree, SymbolToIndexMap *hashtable, bool isMulti);

    void CalcVariation(ParseTree &parsetree, SymbolToIndexMap *hashtable, bool isMulti);

    void CalcCmu();

    void CalcEntRate();

    double getRelEnt() { return m_relEnt; }

    double getCMu() { return m_cMu; }

    double getEntRate() { return m_entRate; }

    void PrintOut(
        char input[],
        const char *,
        const char *,
        const int &,
        const double &,
        const bool &,
        const bool &,
        int);

    void PrintDot(char input[], char alpha[]);

private:
    double m_relEnt;         //relative entropy of machine
    double m_variation;      //distance measure for machine and data
    double m_cMu;            //statistical complexity of machine
    double m_entRate;        //entropy rate of machine
    double m_relEntRate;     //relative entropy per symbol
    AllStates *m_allstates;  //pointer to array of states
};

#endif
