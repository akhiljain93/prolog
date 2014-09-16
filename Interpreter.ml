open Types
open PrologParser

exception BREAK

type substitution = (string * tree) list

let statements = ref []
let temp = ref Empty
let v = ref 0
let temp1 = ref [Empty]
let cut = Hashtbl.create 10
let matchid = ref 0
let stack :((tree Queue.t)*substitution*int) Stack.t = Stack.create()

let rec subst s t = let rec find v s = match s with
					| [] -> Var(v)
					| (a,b)::xs -> 	if a = v
									then b
									else find v xs
					in match t with
					   | Var(a) -> find a s
					   | APred(a,b) -> APred(a, List.map (subst s) b)
					   | Rule(a,b) -> Rule(subst s a, List.map (subst s) b)
					   | _ -> t

let rec mgu t1 t2 = let rec occurs v t = match t with
					| Var(a) -> a = v
					| APred(a,b) -> List.exists (occurs v) b
					| Rule(a,b) -> List.exists (occurs v) (a::b)
					| _ -> false
					in let mguhelp s t1 t2 = s @ mgu (subst s t1) (subst s t2)
					   in match (t1,t2) with
						  | (Var(x), t)
						  | (t, Var(x)) -> if (occurs x t)
						   				   then failwith ""
							   			   else [(x, t)]
						  | (APred(a,b), APred(c,d)) -> if a <> c
						  								 then failwith ""
							   							 else List.fold_left2 mguhelp [] b d
						  | (Rule(a,b), Rule(c,d)) -> List.fold_left2 mguhelp [] (a::b) (c::d)
						  | (Int(a), Int(b)) -> if a=b
				   	   							then []
				   	   							else failwith("")
				   		  | (Real(a), Real(b)) -> if a=b
				   	   							  then []
				   	   							  else failwith("")
				   		  | (Const(a), Const(b)) -> if a=b
				   	   								then []
				   	   								else failwith("")
						  | _ -> failwith ""
								
let subTemp t = let rec naming sub t = let t = subst !sub t
				  	   				   in match t with
						  				  | Var(a) -> if (a.[0] <> '_')
													  then (sub := ((a, Var("_G"^(string_of_int !v))) :: !sub);
															v := !v + 1)
										  | APred(a,b) -> List.iter (naming sub) b
										  | Rule(a,b) -> List.iter (naming sub) (a::b)
										  | _ -> ()
				in let s = ref []
				   in naming s t;
					  !s

let getMBack s = let rec changeVariables c s = let rec substitutedSubstitution (st,t) sub = match sub with
											   | [] -> []
											   | (a,b)::xs -> if (a=st)
															  then substitutedSubstitution (st,t) xs
															  else (a, subst [(st,t)] b)::(substitutedSubstitution (st,t) xs)
											   in match c with
												  | [] -> s
												  | (a,b)::cs -> if a.[0] = '_'
								 								 then let s = substitutedSubstitution (a,b) s
								 									  in changeVariables s s
								 								 else changeVariables cs s
				 in let rec changeOtherWay c s = let rec substitutedSubstitutions (s,t) sub = match sub with
												 | [] -> []
												 | (a,b)::xs -> if (t = Var(a))
																then substitutedSubstitutions (s,t) xs
																else (a, subst [(s,t)] b)::(substitutedSubstitutions (s,t) xs)
												 in match c with
													| [] -> s
													| (a, Var(x))::cs -> if x.[0] = '_'
								 										 then let s = substitutedSubstitutions (x,Var(a)) s
								 											  in changeOtherWay s s
								 										 else changeOtherWay cs s
								 					| x::xs -> changeOtherWay xs s
					in let cv = changeVariables s s
					   in changeOtherWay cv cv

let rec unifyFact p (qi,sub) fact = match p with
| Cut(i) -> let rec iter i = if i < !matchid
							 then (Hashtbl.add cut i true;
								   iter (i+1))
            in iter i;
               Stack.push (qi, sub, !matchid) stack
| APred(a,b) -> (let unifyOthers q s qs = let qs = Queue.copy qs
				 						  in while not(Queue.is_empty qs)
											 do Queue.add (subst s (Queue.take qs)) q
											 done
				 in match fact with
					| [] -> ()
					| (APred(c,d) as ap)::facts -> (try let st = subTemp ap
														in let subs = subst st ap
								   						   in let s = mgu subs (APred(a,b))
								   							  in let q = Queue.create()
								   								 in unifyOthers q s qi;
																	Stack.push (q, sub@s, !matchid) stack;
													with Failure f -> ());
								   				   unifyFact (APred(a,b)) (qi,sub) facts
					| (Rule((APred(c,d) as ap), e))::facts -> (try let st = subTemp ap
												  			   in let subs = subst st ap
												  				  in let s = mgu subs (APred(a,b))
												  					 in let q = Queue.create()
												  						in let add t = Queue.add t q
												  						   in let rec substi sub l = match l with
																			  | [] -> []
																			  | Cut(i)::xs -> Cut(!matchid)::(substi sub xs)
																			  | x::xs -> let subSub = subst sub x
																						 in let subVar = subTemp subSub
																							in let p = (subst s (subst subVar subSub))
																							   in p::(substi (sub@subVar) xs)
																			  in List.iter add (substi st e);
																				 unifyOthers q s qi;
																				 Stack.push (q, sub@s, !matchid) stack;
															   with Failure f -> ());
												  			  unifyFact (APred(a,b)) (qi,sub) facts
					| _ -> failwith "ye kya hua? kaise hua? kyon hua?")
| _ -> failwith "ye to bilkul nahi banta hona"

let print_string s = print_string s; flush stdout

let rec print_tree t = match t with
| Cut(a) -> print_string "Cut "; print_int a
| Var(a) -> print_string a
| Int(a) -> print_int a
| Real(a) -> print_float a
| Const(a) -> print_string a
| APred(a,b) -> let rec print_tl tl = match tl with
				| [] -> ()
				| [x] -> print_tree x
				| x::xs -> print_tree x;
						   print_string ", ";
						   print_tl xs
				in print_string (a^"(");
				   print_tl b;
				   print_string ")"
| _ -> ()

let rec print_result s = let sub = getMBack s
						 in let rec print sub1 = match sub1 with
							| [] -> print_endline "true."
							| [(a,b)] -> print_string (a^" = ");
										 print_tree b
							| (a,b)::xs -> print_string (a^" = ");
										   print_tree b;
										   print_endline ",";
										   print xs
							in print sub;
						 let action = PrologParser.actions PrologScanner.scanner (Lexing.from_channel stdin)
						 in match action with
							| More -> solve()
							| Stop -> ()
and solve () = if (Stack.is_empty stack)
			   then print_endline "false."
			   else let (q,s,id) = Stack.pop stack
			   		in if (Hashtbl.mem cut id)
			   		   then solve()
			   		   else if (Queue.is_empty q)
							then print_result s
							else let elt = Queue.take q
								 in matchid := !matchid + 1;
									unifyFact elt (q,s) !statements;
									solve()

let getProgram () =	try	let lexbuf = Lexing.from_channel stdin
						in while true
						   do try print_string "\nc- ";
	  							  temp := PrologParser.program PrologScanner.scanner lexbuf;
	  							  if !temp <> Empty
	  							  then statements := !temp :: !statements
	  						  with End_of_file -> raise BREAK;
	    						|  PrologScanner.Unrecognised -> flush stdout;
						   done;
					with BREAK -> ()							

let getQueries () = try	let lexbuf = Lexing.from_channel stdin
						in while true
						   do try print_string "\nq- ";
	  							  flush stdout;
	  							  temp1 := PrologParser.queries PrologScanner.scanner lexbuf;
	  							  if !temp1 <> [Empty]
	  							  then (Stack.clear stack;
	  							  		let q = Queue.create()
	  							  		in let add t = Queue.add t q
	  							  		   in List.iter add !temp1;
	  							  		Stack.push (q,[],0) stack;
	  							  		solve())
	  						  with End_of_file -> raise BREAK
	    						 | PrologScanner.Unrecognised -> flush stdout
	    						 | Failure s -> print_string s; flush stdout
						   done;
					with BREAK -> ()

let _ = Printexc.print getProgram ()
let _ = Printexc.print getQueries ()
