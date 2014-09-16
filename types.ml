type tree = Empty
		  | Int of int
		  | Real of float
		  | Cut of int
		  | Var of string
		  | APred of (string*(tree list))
		  | Rule of (tree*(tree list))
		  | Const of string

type action = Stop
			| More