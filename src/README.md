Usage
--------------------
CSSR is a command-line program.

  CSSR alphabetfile datafile maxlength [-m] [-s siglevel] [-ch]

The first argument is the name of a file which contains all the symbols in the
alphabet used by your data. The second argument is the name of the data file
itself. Only one data file can be used, but it may contain multiple lines.
The third argument is the maximum length of history to examine, which we will
abbreviate by L here. The three trailing flags are optional arguments. Set
the -m flag if the data-file contains multiple lines (see below). If the -s
flag is set, it must be followed by a significance level; the default value is
0.001. (For more on setting parameters, see Section 4.) If the -ch flag is
set, the program will use the chi-squared significance test, rather than the
default Kolmogorov-Smirnov test.

In multiple-line mode (entered through the -m flag), each line of the data file
is treated as a distinct time series from the same source. (Technically, as
independent realizations of a single stochastic process.) The lines need not
be of equal length.


Some Details on the Code
----------------------
There are twelve classes in the program. They are each comprised of a .cpp file
and a .h file, except for `ArrayElem` (contained in `G_Array`) and `StringElem`
(contained in `State`), as well as a source file `Main`, and header files
`Common.h` and `Main.h`.

Class    | Description
------------|------------------------------------------------------------------
`AllStates` | Contains and manipulates growable array of states
`ArrayElem` | Each element in the growable array
`G_Array`  | A generic growable array
`Hash`   | Hash table which points from histories to their parent states
`Hash2`   | Hash table which points from indices to symbol/alpha values
`Machine`  | Manipulates the determinized state machine and runs metrics
`ParseTree` | Reads in data file and stores all strings present in file up to length L (the maximum length input at the command line)
`States`  | A state, contains all data for a single state
`StringElem`| The element containing the history for a single state
`Test`   | Performs statistical significance tests
`TransTable`| Stores initially estimated transitions from all histories of length L in any given state. This class is used by AllStates to check for transient states before determinization

For brief descriptions of the classes, see the top of their source files. Note
that the terms "string" and "history" are used interchangeably in the program.
The terms both correspond to the concept of a "history" (as described in
BC), but the program implements these as strings of symbols.

Also, after initial state splitting, all strings/histories of less than maximum
length minus one are deleted. This has no effect on the outcome of the
algorithm and saves time and space.

Lastly, the removal of transient states prior to determinization implemented in
the AllStates::CheckConnComponents procedure is not strictly necessary. With a
different implementation, the deletion of these states could automatically
occur during the determinization process itself (see the pseudocode in BC
for details), and the outcome of the algorithm would be the same. As it is
implemented here the code is slightly redundant.

Known Issues
------------------------
There are a few known issues with CSSR's behavior. These arise from certain
unavoidable approximations in the determinization procedure. (For details, see
below.)

After creating and determinizing the states, CSSR goes over the input data and
attempts to produce the corresponding sequence of causal states, both as a
filtering procedure, and as part of estimating the fit of the causal-state
model (see Section 3, item (3)). Typically there will be some initial number of
symbols which it must read before "synchronizing" to one of the causal states,
after which it will follow the transitions deterministically, and never
de-synchronize. Because of the approximations mentioned, it can happen that
certain transitions are deleted from the causal state model which shouldn't be.
This can lead to three kinds of problem: (1) CSSR can never synchronize to a
state at all; (2) it has to re-synchronize at some point; (3) it encounters an
apparently "impossible" sequence in the data.

In case (1), CSSR writes the message "Error: Could not synchronize, cannot use
this data" to standard error and halts execution. In case (2), it produces a
warning message on both standard error and the info output file. In case (3),
it produces a warning, and discounts that particular string from various
calculations.

The best approach to these problems is to use a longer history length, if
possible, and to provide more data. In the case of a third error, it can
sometimes arise if a particular string appears only once, at the very beginning
of the data, and sometimes removing that string from the data-file fixes
matters.

All three errors arise because we have only finite-length histories available
to us, while what we want are really infinite ones. This forces us to make
certain approximations in our implementation of the theory. Specifically, in
the determinization procedure, we are forced to "guess" which state certain
strings belong to, even though we have not directly examined these strings.
The particular approximation scheme (or "closure") we have adopted may be
investigated by consulting the code in AllStates.cpp. (Others are possible, but
this one seemed to give the best over-all results.) Sometimes this closure will
"guess wrong", and possible transitions will be labeled forbidden, etc. In
these cases, extending the history length _should_ solve the problem, if enough
data is available for reliable inference. Similar approximations must be made
in determining whether or not a given state is transient or recurrent on the
basis of finite data. This occasionally leads to a recurrent state being
labeled transient, which in turn is the most common cause of the code mistaking
an actually-occurring string for an impossible one. Again, the best approach
is to provide more data, and a longer history.


