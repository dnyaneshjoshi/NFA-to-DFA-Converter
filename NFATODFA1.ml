
(* to find dfa states on single state of previous list
# findCombOn1PrevListNode [1] [1;2;3];;          
- : int list list = [[1; 2]; [1; 3]]
*)
let findCombOn1PrevListNode prevListState lst  =
   let lastvalue = List.nth prevListState ((List.length prevListState)-1) in 

     let rec findNextLevel tempList =
        match tempList with 
        [] -> []
        | h :: t ->  if (not (List.mem h prevListState) && ( h > lastvalue)) then ((prevListState @ [h]) :: (findNextLevel t))
             else (findNextLevel t)             
             in findNextLevel lst;;
            
             

(* to find n length states list w.r.t n-1 length (prevlist) states.

# getNLengthList [[1];[2];[3]] [1;2;3];;  
- : int list list = [[1; 2]; [1; 3]; [2; 3]]
*)
let rec getNLengthList prevlist lst =

      match prevlist with 
      []-> []
      |h::t ->  (findCombOn1PrevListNode h lst) @ (getNLengthList t lst) ;;


(* to find dfa states of length one. 
   getLists [1;2;3];;
- : int list list list = [[[1]; [2]; [3]]]
*)

let getLists lst =
  let rec findListLists lst =
  match lst with 
  [] -> []
  | h :: t -> [h] :: findListLists t in  
  [findListLists lst];;


 (*  
    to get dfa's states list of same length in one list. 

         # getstates [1;2;3];;
       - : int list list list =[[[1]; [2]; [3]]; [[1; 2]; [1; 3]; [2; 3]]; [[1; 2; 3]]]
 *)

let getstates lst =
     let rec getSubsets n lst acc=
     if (n <= List.length lst ) then                             
                if n=1 then getSubsets (n+1) lst (getLists lst)     
                else  getSubsets (n+1) lst ((getNLengthList (List.nth acc 0) lst):: acc)          
     else acc
     in List.rev (getSubsets 1  lst []);;
     
     

 
  (* to find target dfa states by finding superset of given nfa states ,      
        #  allCombinations [1; 2; 3];;
- : int list list = [[0]; [1]; [2]; [3]; [1; 2]; [1; 3]; [2; 3]; [1; 2; 3]]

 *)
    
let allCombinations lst =       
      let rec merge lsts=
       match lsts with 
       [] -> [] 
       | h :: t -> h @ merge t
       in [0] :: (merge (getstates lst));;     




(*
getNewTransistion 1  "a" [(1, "a", [2]);(1, "a", [3]);(2, "a", [3]);(3, "a", [3]);(3, "b", [3])];;
- : int list = [2; 3]

*)

let getNewTransistion newstate alphabet prevtransisions = 

     let rec findtransistions newtransistions acc =

        match newtransistions with 
        [] -> acc
        | (a,b,c)::t -> if ((a = newstate) && (alphabet = b)) then  (findtransistions t (c @ acc))
                        else (findtransistions t acc)
           
       in List.rev (findtransistions  prevtransisions []);;

(*
# findNewtransistion3 [1;2;3] "a" [(1, "a", [2]);(1, "a", [3]);(2, "a", [3]);(3, "a", [3]);(3, "b", [3])];;
- : int list = [2; 3; 3; 3]
*)
       
let rec findNewtransistion3 newstates alphabet oldtransations=

   match newstates with 
     [] -> []
     | h :: t ->  (getNewTransistion h alphabet oldtransations) @ (findNewtransistion3 t alphabet oldtransations)
;;
       
       
(*findNewtransistion2 [1;2;3] ["a";"b"] [(1, "a", [2]);(1, "a", [3]);(2, "a", [3]);(3, "a", [3]);(3, "b", [3])];;
- : (int list * string * int list) list =
[([1; 2; 3], "a", [2; 3; 3; 3]); ([1; 2; 3], "b", [3])]
*)
      
let rec findNewtransistion2 newstates alphabet oldtransations=

   match alphabet with 
     [] -> []
     | h :: t ->  (newstates, h ,(findNewtransistion3 newstates h oldtransations)) :: (findNewtransistion2 newstates t oldtransations)
;;

(*  findTransistions [[0]; [1]; [2]; [3]; [1; 2]; [1; 3]; [2; 3]; [1; 2; 3]] ["a";"b"] [(1, "a", [2]);(1, "a", [3]);(2, "a", [3]);(3, "a", [3]);(3, "b", [3])];;
- : (int list * string * int list) list =
[([0], "a", []); ([0], "b", []); ([1], "a", [2; 3]); ([1], "b", []);
 ([2], "a", [3]); ([2], "b", []); ([3], "a", [3]); ([3], "b", [3]);
 ([1; 2], "a", [2; 3; 3]); ([1; 2], "b", []); ([1; 3], "a", [2; 3; 3]);
 ([1; 3], "b", [3]); ([2; 3], "a", [3; 3]); ([2; 3], "b", [3]);
 ([1; 2; 3], "a", [2; 3; 3; 3]); ([1; 2; 3], "b", [3])]*)

let rec findTransistions newstates alphabet oldtransations=

   match newstates with 
   [] -> [] 
   | h :: t ->  (findNewtransistion2 h alphabet oldtransations) @  (findTransistions t alphabet oldtransations);;
       


(*replace []  with [0] in newtransistions *)
let rec refine transistions =

     match transistions with 
     []-> []
     | (a,b,c) :: t -> if c=[] then (a,b,[0]) :: (refine t)
                  else (a,b,c) :: (refine t);;


(*# removeduplicates [2; 3; 3; 3];;
- : int list = [2; 3]
*)
let removedupsinlst lst =

    let rec innerhelperremoveduplicates lst1 acc = 
    match lst1 with 
    [] -> acc 
    | h :: t -> if (List.mem h acc) then  innerhelperremoveduplicates t acc 
                else innerhelperremoveduplicates t (h :: acc)  
    
     in List.sort (fun x y -> if x > y then 1 else 0) (innerhelperremoveduplicates  lst []) ;;

(*to remove duplications in newtransistions*)
let rec removeduplicates transistions =

     match transistions with 
    []-> []
    | (a,b,c) :: t -> (a,b,removedupsinlst c) :: (removeduplicates t);;


(*helper function to find final states in dfa*)

let  rec checkIsFinal finalstates newstate =

 match finalstates with 
 [] -> false
  | h :: t -> if (List.mem h newstate ) then true 
              else checkIsFinal t newstate ;;
              
 (*to find final states in dfa *)
let rec findFinalstates finalstates newstates=  
     
        match newstates with 
        [] -> []
        | h :: t -> if (checkIsFinal finalstates h ) then h :: findFinalstates finalstates t
                    else findFinalstates finalstates t       
   
    ;;
    
    
    
type nfaType ={
nfaStates : int list;
nfaAlphabet : string list;
nfaTransistions :(int * string * int list) list;
nfaStartState : int;
nfaFinalstate : int list
}


type dfaType ={
dfastates : int list list;
dfaalphabet : string list;
dfatransistions :(int list * string * int list) list;
dfastartstate : int list;
dfafinalstate : int list list
}




(*

# reachablesOnOneAlphabet [1;2;3] dfaType.dfatransistions "a";;         
- : int list = [1; 2; 3]
*)

let rec reachablesOnOneAlphabet state transistions alphabet =
    match transistions with 
    []->[]
    | (a,b,c) :: t ->  if (a=state && b=alphabet) then c 
                       else reachablesOnOneAlphabet state t alphabet;;


(*# reachablestates [1;2;3] dfaType.dfatransistions dfaType.dfaalphabet;;
- : int list list = [[1; 2; 3]; [3]]
*)
let rec  reachablestates state transistions inputalphabet=
    match inputalphabet with 
    [] -> []
    | h :: t -> (reachablesOnOneAlphabet state transistions h) :: reachablestates state transistions t;;   


(*to check whether newly find state is already traversed or not *)
let rec alreadyTraced acc currentstate newstates =

     match newstates with 
     [] -> []
     | h :: t -> if (List.mem h acc) || (currentstate = h) then alreadyTraced acc currentstate t
                 else h :: alreadyTraced acc currentstate t;;

(* to remove states which are not reachable from start state*)
let rec refinestates transistions acc stack inputalphabet =
    match stack with     
    [] -> acc
    | h :: t ->      
           let newstates = reachablestates h transistions inputalphabet in           
           let toaddTostack = alreadyTraced acc h newstates in 
            refinestates transistions (h :: acc) (t @ toaddTostack) inputalphabet;;


(* to remove unnecessary states  in dfa*)

let rec removeStates refinedstates allstates =

    match allstates with 
    [] -> [] 
    | h :: t ->  if (List.mem h refinedstates) then h :: removeStates refinedstates t 
              else removeStates refinedstates t ;;
              
              
(* to remove unnecessary final states  in dfa *)
let rec removeFinal refinedstates finalstates =
    match finalstates with 
    [] -> [] 
  | h :: t ->  if (List.mem h refinedstates) then h :: removeStates refinedstates t 
              else removeStates refinedstates t ;;

 (* to remove unnecessary transistions in dfa *)             
let rec removeTransistions refinedstates alltransistions =

 match alltransistions with 
  [] -> []
  | (a,b,c)  :: t -> if (List.mem a refinedstates) then (a,b,c) :: removeTransistions refinedstates t
                     else removeTransistions refinedstates t ;;


    
let dfa nfainput= 
   let newstates=allCombinations nfainput.nfaStates in 

     let newtransistions = findTransistions newstates nfainput.nfaAlphabet nfainput.nfaTransistions in 

      let withdeadstate = refine newtransistions in 

       let newtrans= removeduplicates withdeadstate  and finalstates= (findFinalstates nfainput.nfaFinalstate newstates) in 
   
       let dfaoutput = {
         dfastates = newstates ;
         dfaalphabet = nfainput.nfaAlphabet;
         dfatransistions =newtrans;
         dfastartstate = [nfainput.nfaStartState]; 
         dfafinalstate = finalstates
        } in 
        
       let refinedstates= refinestates dfaoutput.dfatransistions [] [dfaoutput.dfastartstate] dfaoutput.dfaalphabet     
       
          in  
         {
         dfastates = removeStates refinedstates dfaoutput.dfastates ;
         dfaalphabet = dfaoutput.dfaalphabet;
         dfatransistions =removeTransistions refinedstates dfaoutput.dfatransistions;
         dfastartstate = [nfainput.nfaStartState]; 
         dfafinalstate = removeFinal refinedstates dfaoutput.dfafinalstate ;
        }
   
   

(*Tests and Drivers*)

let test1 = 
   let nfainput1= {
                  nfaStates =[1; 2; 3] ;
                  nfaAlphabet = ["a"; "b"];
                  nfaTransistions =[
    	                         	(1, "a", [2;3]);
    		                        (1, "a", [1;3]);
    		                        (2, "a", [3]);
    		                        (3, "a", [3]);
    		                        (3, "b", [3])
                                ];
                 nfaStartState = 1;
                 nfaFinalstate = [3]
                 } 
          in dfa nfainput1;;



let test2 = 
       let nfainput2= {
                      nfaStates =[1; 2; 3; 4; 5] ;
                      nfaAlphabet = ["a"; "b"];
                      nfaTransistions =[
                         		(1, "a", [1; 2; 3; 4; 5]);
      	                         	(1, "b", [4;5]);
    	                         	(2, "a", [3]);
    		                        (2, "b", [5]);
    		                        (3, "b", [2]);
    		                        (4, "a", [5]);
    		                        (4, "b", [4])
                                	];
                     nfaStartState = 1;
                     nfaFinalstate = [5]

                     }
             in dfa nfainput2;;



   







       
       

        



