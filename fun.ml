open QCheck

(* An example property that is false *)
let prop_false_bool = Test.make ~name:"false bool" ~count:1000
  (pair (fun1 bool bool) bool) (fun (f,b) -> f (f b) = b)

(* An example property from JSVerify https://github.com/jsverify/jsverify *)
let prop_thrice_bool = Test.make ~name:"thrice bool" ~count:1000
  (pair (fun1 bool bool) bool) (fun (f,b) -> f (f (f b)) = f b)

(** Examples from Claessen:Haskell12 *)

(* An example (false) property *)
let prop_map_filter =
  let int_gen = small_nat (* int *) in 
  Test.make ~name:"map filter" ~count:1000
    (triple 
       (fun1 int_gen int_gen)
       (fun1 int_gen bool)
       (list int_gen)) (fun (f,p,xs) -> List.map f (List.filter p xs) = List.filter p (List.map f xs))

(* Another example (false) property *)
let prop_foldleft_foldright =
  let int_gen = small_nat (* int *) in 
  Test.make ~name:"fold_left fold_right" ~count:1000
    (triple 
       (fun2 int_gen int_gen int_gen)
       int_gen
       (list int_gen)) (fun (f,z,xs) -> List.fold_right f xs z = List.fold_left f z xs)

(* Another example (false) property, curried *)
let prop_foldleft_foldright' =
  let int_gen = small_nat (* int *) in 
  Test.make ~name:"fold_left fold_right'" ~count:1000
    (triple
       (fun1 (pair int_gen int_gen) int_gen)
       int_gen
       (list int_gen))
    (fun (f,z,xs) ->
      List.fold_right (fun e acc -> f (e,acc)) xs z = List.fold_left (fun acc e -> f (acc,e)) z xs)

(* A third example (false) property *)
let prop_pred_string =
  Test.make ~name:"pred string" ~count:1000
    (fun1 string bool)
    (fun p ->
      p "some long string" ==> p "some other string")

(* Other examples from Claessen:Haskell12: 
    heap invariants, monad laws *)

(** Examples from Norell: Generating random functions https://vimeo.com/143848099 *)

let prop_member =
  let int_gen = small_nat (* int *) in 
  Test.make ~name:"member" ~count:1000
    (triple
       (fun1 int_gen int_gen)
       int_gen
       (list int_gen))
    (fun (f,x,xs) -> not (List.mem (f x) (List.map f xs)) ==> not (List.mem x xs))

let prop_silly =
  let int_gen = small_nat (* int *) in 
  Test.make ~name:"silly" ~count:1000
    (triple
       (fun1 string int_gen)
       string
       string)
    (fun (f,w1,w2) ->  (* not precisely Norell's example prop (which is dyn.typed) *)
      w1 <> w2 ==> (f w1 + f (List.hd (String.split_on_char ' ' w1)) mod 2 = 0
                 || f w2 + f (List.hd (String.split_on_char ' ' w2)) mod 2 = 0))


(* Norell's Nim example *)
type move = Take2 | Take3
let move_gen = make ~print:(function Take2 -> "take2" | Take3 -> "take3") (Gen.oneofl [Take2; Take3])

let rec play n ((_,strat1) as player1) ((name2,_) as player2) =
  match strat1 n with
    | Take2 when n >= 2 -> play (n-2) player2 player1
    | Take3 when n >= 3 -> play (n-3) player2 player1
    | _ -> name2

(* First test whether a given strategy is a winning one *) 
let prop_winning name n strat =
  let int_gen = small_nat (* int *) in 
  Test.make ~name:name ~count:1000
    (fun1 int_gen move_gen)
    (fun s -> "win" = play n ("win",strat) ("lose",s))

(* Another example from Norell: prop_losing *)

;;
QCheck_runner.run_tests ~verbose:true
  [prop_false_bool;
   prop_thrice_bool;
   (* Claessen *)
   prop_map_filter;
   prop_foldleft_foldright;
   prop_foldleft_foldright';
   prop_pred_string;
   (* Norell *)
   prop_member;
   prop_silly;
   prop_winning "nim: always take2" 19 (function _ -> Take2);
  ]
