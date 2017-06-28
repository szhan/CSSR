CSSR
=======

Introduction
--------------
This is a R wrapper around the CSSR program originally written by Kristina Klinkner and now maintained by Sam Stites. The CSSR theory and algorithm were developed by Cosma Shalizi, Kristina Klinkner, and James Crutchfield.

Description of the Algorithm
--------------
CSSR tries to infer the minimal Markovian model capable of generating a
time-series, or set of time-series from the same source. The program implements
the algorithm proposed in the paper, hereafter BC, _[Blind Construction of
Optimal Nonlinear Recursive Predictors for Discrete Sequences][arxiv]_
<sup>1</sup>.  We won't describe the algorithm in any detail here (see the paper
for that), but the next two paragraphs say a little about what it produces and
  how it works at a high level.

The output of the algorithm is a set of states which form a Markov chain. Each
state has a certain probability of emitting any of the symbols in the original
time series. The current state and the symbol it emits fix the next state.  (The
states are "future-resolving", if you're from nonlinear dynamics, or
"deterministic", if you're from automata theory.) Each state, moreover,
corresponds to a distinct set of strings, in the following sense. If the state A
contains a string w, and at time t the time-series ends with w, then at time t
the Markov chain is in state A. The set of states, their transition
probabilities and connections, is called the state machine.

The algorithm uses a recursive inference procedure to find the simplest set of
states with the above properties that can reproduce the statistical properties
of the data. If we could give the algorithm an infinitely long time series, and
let it consider infinitely long sub-strings, it would produce the causal states
of the process, which are its ideal predictors (see BC for a formal definition).
Since we have only finite data, there is always some probability that the
inferred or estimated states are not the true causal states.  Nonetheless, for
the rest of this file, when we say "causal states", we mean the estimated causal
states.

---

<sup>1</sup> _Blind Construction of Optimal Nonlinear Recursive Predictors for
Discrete Sequences_, Cosma Rohilla Shalizi and Kristina Lisa Shalizi, pp.
504--511 of Max Chickering and Joseph Halpern (eds.), _Uncertainty in Artificial
Intelligence: Proceedings of the Twentieth Conference_. [link][arxiv]

[arxiv]: http://arxiv.org/abs/cs.LG/0406011


Installing the Original Program
--------------

There are several versions of CSSR in this repository. The original code,
written in C++, can be found in the `cpp` folder. This is not actively
maintained, however it is stable and is the tried-and-true implementation.
Deployment to different package managers will happen further down the line,
for now:

    # the recursive flag is optional
    git clone https://github.com/stites/CSSR.git --recursive
    cd CSSR/cpp
    make                   # warnings can be safely ignored
    cp CSSR ~/.local/bin   # whereever you keep your binaries

Details of how to use this program can be found in [cpp/README.md](cpp/README.md).


Preview v0.2.0
--------------
Cosma, Kristi, and I are currently working on large improvements for CSSR. The
source for this can be found in the `scala` folder. If you would like to preview
this implementation, a release candidate can be [found at github][v0.2.0-RC1].

From the command line:

    curl -LO https://github.com/stites/CSSR/releases/download/v0.2.0-RC1/cssr-assembly-0.2.0-RC1.jar
    java -jar cssr-assembly-0.1.0.jar --alphabet test-machines/perl/alphabets/binary --data tmp/Misiurewicz_timeseq
    # where tmp/Misiurewicz_timeseq is generated from test-machines/perl/misiurewicz.pl

Output
--------------------

The program will create the following four files after running, where
`<dataname>` is the name of the file the data is in:

  1. `<dataname>_results` - contains the information on the inferred states and
     their properties. For each state, it gives:
      * the histories of length `L-1` and `L` in the state
      * the probability that the state will emit different symbols
        (e.g. `P(a) = x`) and the states transitioned to when those symbols are
        emitted (e.g.  `T(a) = s`)
      * the observed probability of the state in the data-stream
  2. `<dataname>_state_series` - is the series of causal states in the data.
     That is, the program scans through the data, looks up which state the
     history-to-date is in, and writes the corresponding symbol to this file.
     What you see is then the trajectory through estimated causal state space of
     the data/process. Multiline data results in a multiline state-series file.
  3. `<dataname>_inf.dot` - represents the state machine as a labeled directed
     graph, where each state has its own node, and each transition between
     states its own edge. The file is for use with the program dot, available
     from [graphviz.org](http://www.graphviz.org).
  4. `<dataname>_info` - is the file containing the command-line settings and
     the metrics run on the causal state machine. These are:
      * the name of the alphabet file
      * the alphabet size
      * the name of the data file
      * the history length
      * the significance level
      * whether multi-line mode was used
      * whether the chi-squared test was used
      * the number of states
      * the statistical complexity (entropy of the states)
      * the entropy rate
      * three measures of the difference between the empirical distribution of
        symbol sequences, and that generated by the inferred causal state
        machine. These are:
        + the divergence or relative entropy between the inferred and empirical
          distribution*
        + the relative entropy rate, or increase per symbol in the divergence*
        + the total variational distance ("variation") between the two
          distributions.

*Note that the relative entropy and the relative entropy rate can be infinite;
this indicates that the inferred model gives a probability of zero to a
sequence in the data.

*Note that sometimes, when the relative entropy should be very small (order of
1e-6 bits or less), numerical rounding errors result in a negative number being
calculated. In these cases, the program outputs zero. Similarly, the
complexity of one-state machines is sometimes reported as -0.

Some Suggestions About Parameters
------------------------------

It is always good to use as much data as you can. While it is generally good
practice to hold back some data for testing or cross-validation, we recommend
that this be minimized. High-entropy processes are especially data-hungry.
(See BC.) For reference, let us call the number of data-points N.

The two key parameters of the program are the maximum history length, L, and
the significance level used in the test, s. For any given process, there is a
minimum history length M, such that the true states cannot be found if L < M.
The number of states returned may be less than the correct number or higher.
If L >= M, and there is enough data, there will generally be a "plateau" of
values of L where the correct number of states is returned. For fixed N, if we
keep increasing L, then past a certain point there are not enough examples of
each string in the data. This tends to erroneously create new states, which
spawn others through determinization. Thus there is generally a "blow-up" when
L is too large (relative to N and s). A rough guide-line is to limit L to no
more than log(N)/log(k), where k is the alphabet size (see BC for
details).

In general, one should use as small an L as possible, since under-sampling,
even before the blow-up, will reduce the accuracy of many probability
estimates. Blow-up can be delayed by reducing s --- that is, reducing the
probability of mistakenly splitting a state --- but this carries the risk of
failing to create valid new states. We suggest exploring the data at low L and
high s initially, and then increasing L and lowering s. If a stable
architecture is found, it should be recorded at the lowest possible L.


Known Issues
------------------------
There are a few known issues with CSSR's behavior in the C++ code. These arise
from certain unavoidable approximations in the determinization procedure. For
details, see [cpp/README.md#known-issues](cpp/README.md#known-issues). While
v0.2.0 will not have the same limitations as the original C++, these issues
still need to be reviewed in the context of the new algorithm.

Bug Reports, Fixes, Modifications
-----------------------------
We welcome bug reports or reports of strange behavior. These reports are
welcomed with more enthusiasm when accompanied by successful modifications to
the code! (See the accompanying file on the Gnu Public License for information
about modifying the code.) Even if you can't fix it, however, please do tell us
about it; at the least it will be documented for other users.

If you modify CSSR, and want to make the resulting program available, please let
us know. We are happy to provide a link, and have a (limited) capability to host
alternate versions and descendants. Also, if you use CSSR successfully in some
application, we'd love to hear about it.

Please check [AUTHORS.md](AUTHORS.md) for up-to-date contact information.
