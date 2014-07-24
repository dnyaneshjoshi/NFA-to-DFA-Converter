(*
	This OCaml program returns an equivalent DFA 5-tuple
	 given an NFA 5-tuple as the input.
	
	It uses a top-down approach and hence computes only
	 those DFA transitions which need to be computed.
	 Moreover, it computes them only once.
*)


(*Returns a list with distinct elements from the input list.*)
let rec getDistinctElements lst =
	match lst with
	| [] -> []
	| h::t ->
		let rec remove t h =
			match t with
			| [] -> []
			| hd::tl ->
				if hd = h then
					remove tl h
				else
					hd::remove tl h
		in
			h::getDistinctElements (remove t h);;

(*Returns true if element e is found in list lst, else returns false.*)
let rec find lst e =
	match lst with
	| [] -> false
	| h::t ->
		if h = e then
			true
		else
			find t e;;

(*Returns a list of next states of the input NFA on the present state-input symbol combination.*)
let rec getNfaNextStates nfaTransitions state inputSymbol =
	match nfaTransitions with
	| [] -> []
	| h::t ->
		match h with
		| (q, a, l) ->
			if (q = state && a = inputSymbol) then
				l
			else
				getNfaNextStates t state inputSymbol;;

let compare a b =
	if a = b then
		0
	else if a < b then
		-1
	else
		1;;

(*Takes a DFA state and an input symbol as the input and returns the next DFA state.
The returned DFA state is made up of a subset of the power set of the NFA states sorted in the lexicographic order.
Sorting is an easy way to make sure two DFA states which are the same look the same. It also facilitates comparison.*)
let getDfaNextState nfaTransitions state inputSymbol =
	let rec getDfaRawNextState nfaTransitions state inputSymbol =
		match state with
		| [] -> []
		| h::t ->
			let l = getNfaNextStates nfaTransitions h inputSymbol in
				l @ getDfaRawNextState nfaTransitions t inputSymbol
	in
		List.stable_sort compare (getDistinctElements (getDfaRawNextState nfaTransitions state inputSymbol));;

let qDfaStates = ref [];; (*Queue to hold DFA states.*)

let qDfaFinalStates = ref [];; (*Queue to hold DFA final states.*)

let qDfaTransitions = ref [];; (*Queue to hold DFA transitions.*)

let queue = ref [];; (*Queue for internal computation purpose.*)

(*Enqueues element e in queue q only if e is not present in both q and q1.*)
let enqueue q q1 e =
	if (find !q e) || (find !q1 e) then
		()
	else
		q := List.rev (e :: List.rev !q);;

(*Dequeues the element at the head of q.*)
let dequeue q =
	match !q with
	| [] -> []
	| h::t ->
		let e = h in
		(
			q := t;
			e
		);;

(*Reads the element at the head of q without dequing it.*)
let readHead q =
	match !q with
	| [] -> []
	| h::t -> h;;

(*Helper function - Enqueues into queue the next DFA states from the state e on all symbols
in the alphabet.*)
let rec f alphabet nfaTransitions e =
	match alphabet with
	| [] -> ()
	| h::t ->
	(
		enqueue queue qDfaStates (getDfaNextState nfaTransitions e h);
		f t nfaTransitions e
	);;

(*Returns the set of DFA states.*)
let rec getDfaStates alphabet nfaTransitions =
	let e = ref (dequeue queue) in
	while (!e <> [] || (List.length !queue) > 0) do
		if !e <> [] then
		(
			enqueue qDfaStates qDfaStates !e;
			f alphabet nfaTransitions !e
		);
		e := (dequeue queue);
	done;
	!qDfaStates;;

(*Populates qDfaFinalStates with the final states of the DFA.*)
let rec populateDfaFinalStates nfaFinalStates dfaStates =
	match dfaStates with
	| [] -> ()
	| h::t ->
		let rec g nfs =
			match nfs with
			| [] -> ()
			| hh::tt ->
				if find h hh then
					(enqueue qDfaFinalStates qDfaFinalStates h;
					g tt)
				else
					g tt
		in
		(
			g nfaFinalStates;
			populateDfaFinalStates nfaFinalStates t
		);;
			
(*Populates qDfaTransitions with the transitions of the DFA.*)
let rec populateDfaTransitions dfaStates alphabet nfaTransitions =
	match dfaStates with
	| [] -> ()
	| h::t ->
		let rec g a =
			match a with
			| [] -> ()
			| hh::tt ->
				(enqueue qDfaTransitions qDfaTransitions (h, hh, getDfaNextState nfaTransitions h hh);
				g tt)
		in
		(
			g alphabet;
			populateDfaTransitions t alphabet nfaTransitions
		);;

(*Takes an NFA 5-tuple as the input, converts it to a DFA 5-tuple and returns the DFA 5-tuple.*)
let toDfa nfa =
	match nfa with
	| (nfaStates, alphabet, nfaTransitions, startState, nfaFinalStates) ->
		(
			qDfaStates := [];
			qDfaFinalStates := [];
			qDfaTransitions := [];
			queue := [];
			
			enqueue queue qDfaStates (startState::[]);
			let dfaStates = getDfaStates alphabet nfaTransitions in
				(populateDfaFinalStates nfaFinalStates dfaStates;
				populateDfaTransitions dfaStates alphabet nfaTransitions;
				(dfaStates, alphabet, !qDfaTransitions, startState, !qDfaFinalStates))
		);;


(*Tests and Drivers*)

(*An NFA 5-tuple*)
let nfa1 =
(
	["q1"; "q2"; "q3"; "q4"; "q5"], (*SET OF NFA STATES*)
	["a"; "b"], (*INPUT ALPHABET*)
	[ (*SET OF TRANSITIONS*)
		("q1", "a", ["q1"; "q2"; "q3"; "q4"; "q5"]);
		("q1", "b", ["q4"; "q5"]);
		("q2", "a", ["q3"]);
		("q2", "b", ["q5"]);
		("q3", "b", ["q2"]);
		("q4", "a", ["q5"]);
		("q4", "b", ["q4"])
	],
	"q1", (*START STATE*)
	["q5"] (*SET OF FINAL STATES*)
);;

let nfa2 =
(
	["q0"; "q1"; "q2"], (*SET OF NFA STATES*)
	["0"; "1"], (*INPUT ALPHABET*)
	[ (*SET OF TRANSITIONS*)
		("q0", "0", ["q0"; "q1"]);
		("q0", "1", ["q1"]);
		("q1", "0", ["q2"]);
		("q1", "1", ["q2"]);
		("q2", "1", ["q2"])
	],
	"q0", (*START STATE*)
	["q1"] (*SET OF FINAL STATES*)
);;

toDfa nfa1;;
toDfa nfa2;;
