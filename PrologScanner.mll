{
	open PrologParser
	open String
	exception Unrecognised
}

let nZDigit = ['1'-'9']
let digit = ['0'-'9']

rule scanner = parse
| ('0'|((nZDigit)(digit*))) as inum
{	Integer (int_of_string inum)	}
| (('0'|((nZDigit)(digit*)))('.'('0'|digit*nZDigit)?)?(['e' 'E']['~']?('0'|((nZDigit)(digit*))))?) as num
{
	let fnumb = if (contains num '~') then (let ind = (index num '~') in (num.[ind] <- '-'; num)) else num
	in (Reals (float_of_string fnumb))
}
| '.'
{	Dot		}
| ":-"
{	If		}
| ','
{	Comma 	}
| '!'
{	Exclamation(0)		}
| ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '\'' '_']* as variable
{	IdVar variable	}
| ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '\'' '_']* as id
{	Ident id	}
| '('
{	LParentheses	}
| ')'
{	RParentheses	}
| ';'
{	Semi	}
| '\n'
{	Newline 		}
| ' ' | '\t'
{	scanner lexbuf	}
| _
{	print_string "Unrecognised character"; raise Unrecognised	}
| eof
{	raise End_of_file	}

{
}