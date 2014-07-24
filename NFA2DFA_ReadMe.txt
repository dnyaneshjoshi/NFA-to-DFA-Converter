NFA2DFA takes a top-down approach.

Algorithm:
1. Start with the DFA start state.
2. For a given DFA state, find the next DFA state on all input symbols. For the appropriate ones among these states, repeat 2.
3. On each of the states in the list of states given by step 2, find the transition on all input symbols.
4. Find those states in the list of states given by step 2 which contain at least one of the NFA final states.

Here,
- Step 2 computes the list of DFA states.
- Step 3 computes the DFA transitions, which is a list of tuples each of which is of the form
	(the present DFA state, the input symbol, the next DFA state)
- Step 4 computes the list of DFA final states.
- The DFA input alphabet is the same as the NFA input alphabet.
- The DFA start state is a list containing only one element - the NFA start state.

__________________________________________________________________________________________________________________


NFATODFA1 takes a brute-force approach.

Algorithm:
1. Start with finding all subsets of given nfa states, let us consider the subsets will be new states of target DFA.
2. for each newstate find the transistions with the given input nfa transisitons.
3. from the new transistions , start from the initial state and include all the states which can be reachable.
4. find the states which contain the final nodes of given nfa, which forms final states of target DFA. 


Step1 corresponds ->  function allCombinations
step2 corresponds -> function findTransistions
step3 correspoonds -> function refinestates
step4 corresponds -> function removeFinal


_______________________________________________________________________________________________________________________________



Execution :


#use "NFA2DFA.ml"

or 

#use "NFATODFA1.ml"







