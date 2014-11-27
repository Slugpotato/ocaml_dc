(*

;; STUDENTS
;;    Nicholas Wood
;;    William Cork
;;

*)

include Scanner
include Bigint

open Bigint
open Printf
open Scanner

type stack_t = Bigint.bigint Stack.t
let push = Stack.push
let pop = Stack.pop

let registers = Hashtbl.create 10

let ord thechar = int_of_char thechar
type binop_t = bigint -> bigint -> bigint

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let print_number number = 
    let bignumstring = explode (string_of_bigint number)
    in let rec print_number' number counter =
       match (number, counter) with
       | [],_          -> ()
       | c::d,69       -> 
            printf "\\\n%c" c;
            print_number' d 1
       | c::d,_        ->
            printf "%c" c;
            print_number' d (counter+1)
    in print_number' bignumstring 0;
       printf "\n%!"

let print_stackempty () = eprintf "stack empty\n%!"

let executereg (thestack: stack_t) (oper: char) (reg: int) =
    try match oper with
        | 'l' -> 
            if (Hashtbl.mem registers reg)
            then push (Hashtbl.find registers reg) thestack
            else printf "register '%c' (0%o) is empty\n%!"
                                            (char_of_int reg) reg
        | 's' ->
            Hashtbl.replace registers reg (pop thestack)
        | _   -> printf "0%o 0%o is unimplemented\n%!" (ord oper) reg
    with Stack.Empty -> print_stackempty()

let executebinop (thestack: stack_t) (oper: binop_t) (operc: char) =
    try let right = pop thestack
        in  try let left = pop thestack
                in  if (operc = '/')
                       && ((Bigint.cmp right Bigint.zero) = 0)
                    then (eprintf "divide by zero\n%!";
                         push left thestack;
                         push right thestack)
                    else push (oper left right) thestack
            with Stack.Empty -> (print_stackempty ();
                                push right thestack)
    with Stack.Empty -> print_stackempty ()

let execute (thestack: stack_t) (oper: char) =
    try match oper with
        | '+'  -> executebinop thestack Bigint.add oper
        | '-'  -> executebinop thestack Bigint.sub oper
        | '*'  -> executebinop thestack Bigint.mul oper
        | '/'  -> executebinop thestack Bigint.div oper
        | '%'  -> executebinop thestack Bigint.rem oper
        | '^'  -> executebinop thestack Bigint.pow oper
        | 'c'  -> Stack.clear thestack
        | 'd'  -> push (Stack.top thestack) thestack
        | 'f'  -> Stack.iter print_number thestack
        | 'l'  -> failwith "operator l scanned with no register"
        | 'p'  -> print_number (Stack.top thestack)
        | 's'  -> failwith "operator s scanned with no register"
        | '\n' -> ()
        | ' '  -> ()
        | _    -> printf "0%o is unimplemented\n%!" (ord oper)
    with Stack.Empty -> print_stackempty()

let toploop (thestack: stack_t) inputchannel =
    let scanbuf = Lexing.from_channel inputchannel in
    let rec toploop () = 
        try  let nexttoken = Scanner.scanner scanbuf
             in  (match nexttoken with
                 | Number number       -> push number thestack
                 | Regoper (oper, reg) -> executereg thestack oper reg
                 | Operator oper       -> execute thestack oper
                 );
             toploop ()
        with End_of_file -> printf "%!";(*printf "End_of_file\n%!";*)
    in  toploop ()

let readfiles () =
    let thestack : bigint Stack.t = Stack.create ()
    in  ((if Array.length Sys.argv > 1 
         then try  let thefile = open_in Sys.argv.(1)
                   in  toploop thestack thefile
              with Sys_error message -> (
                   printf "%s: %s\n%!" Sys.argv.(0) message;
                   exit 1));
        toploop thestack stdin)

let interact () =
    let thestack : bigint Stack.t = Stack.create ()
    in  toploop thestack stdin

let _ = if not !Sys.interactive then readfiles ()

