open! Core
open! Import
module Token_pl = Util.Common_lexer.Programming_language.Token

let lex_pl input = Util.Common_lexer.Programming_language.lex ~input ~filename:"file.txt"

let%expect_test "underscore" =
  let tokens =
    lex_pl
      {|
_ = 5 in ();
|}
  in
  print_s
    [%message (tokens : Token_pl.t Source_position.With_section.t list With_errors.t)];
  [%expect
    {|
    (tokens
     ((value
       (((value (Definition Underscore)) (section file.txt:1,0-1,1))
        ((value (Symbol (Operator (Base (Equal))))) (section file.txt:1,2-1,3))
        ((value (Constant (Int 5))) (section file.txt:1,4-1,5))
        ((value (Definition In)) (section file.txt:1,6-1,8))
        ((value (Grouping (Parenthesis Left))) (section file.txt:1,9-1,10))
        ((value (Grouping (Parenthesis Right))) (section file.txt:1,10-1,11))
        ((value (Symbol Semicolon)) (section file.txt:1,11-1,12))))
      (errors ())))
    |}]
;;

let%expect_test "constants" =
  let tokens =
    lex_pl
      {|
int = 5 in
bool = true in
float = 2.71 in
char = '\n' in
string = "
  hello world
" in
();
|}
  in
  print_s
    [%message (tokens : Token_pl.t Source_position.With_section.t list With_errors.t)];
  [%expect
    {|
    (tokens
     ((value
       (((value (Identifier ((name int)))) (section file.txt:1,0-1,3))
        ((value (Symbol (Operator (Base (Equal))))) (section file.txt:1,4-1,5))
        ((value (Constant (Int 5))) (section file.txt:1,6-1,7))
        ((value (Definition In)) (section file.txt:1,8-1,10))
        ((value (Identifier ((name bool)))) (section file.txt:2,0-2,4))
        ((value (Symbol (Operator (Base (Equal))))) (section file.txt:2,5-2,6))
        ((value (Constant (Bool true))) (section file.txt:2,7-2,11))
        ((value (Definition In)) (section file.txt:2,12-2,14))
        ((value (Identifier ((name float)))) (section file.txt:3,0-3,5))
        ((value (Symbol (Operator (Base (Equal))))) (section file.txt:3,6-3,7))
        ((value (Constant (Float 2.71))) (section file.txt:3,8-3,12))
        ((value (Definition In)) (section file.txt:3,13-3,15))
        ((value (Identifier ((name char)))) (section file.txt:4,0-4,4))
        ((value (Symbol (Operator (Base (Equal))))) (section file.txt:4,5-4,6))
        ((value (Constant (Char "\\n"))) (section file.txt:4,7-4,11))
        ((value (Definition In)) (section file.txt:4,12-4,14))
        ((value (Identifier ((name string)))) (section file.txt:5,0-5,6))
        ((value (Symbol (Operator (Base (Equal))))) (section file.txt:5,7-5,8))
        ((value (Constant (String  "\
                                  \n  hello world\
                                  \n")))
         (section file.txt:5,9-7,1))
        ((value (Definition In)) (section file.txt:7,2-7,4))
        ((value (Grouping (Parenthesis Left))) (section file.txt:8,0-8,1))
        ((value (Grouping (Parenthesis Right))) (section file.txt:8,1-8,2))
        ((value (Symbol Semicolon)) (section file.txt:8,2-8,3))))
      (errors ())))
    |}]
;;

let%expect_test "symbols" =
  let tokens =
    lex_pl
      {|
foo = ((a * b)[c] // d |> e f) && (g ** h ^ i @ j) || ($k <~> l%) in
bar = (m, n) := [o = p; q : r.s::t] in
();
|}
  in
  print_s
    [%message (tokens : Token_pl.t Source_position.With_section.t list With_errors.t)];
  [%expect
    {|
    (tokens
     ((value
       (((value (Identifier ((name foo)))) (section file.txt:1,0-1,3))
        ((value (Symbol (Operator (Base (Equal))))) (section file.txt:1,4-1,5))
        ((value (Grouping (Parenthesis Left))) (section file.txt:1,6-1,7))
        ((value (Grouping (Parenthesis Left))) (section file.txt:1,7-1,8))
        ((value (Identifier ((name a)))) (section file.txt:1,8-1,9))
        ((value (Symbol (Operator (Base (Times))))) (section file.txt:1,10-1,11))
        ((value (Identifier ((name b)))) (section file.txt:1,12-1,13))
        ((value (Grouping (Parenthesis Right))) (section file.txt:1,13-1,14))
        ((value (Grouping (Square_bracket Left))) (section file.txt:1,14-1,15))
        ((value (Identifier ((name c)))) (section file.txt:1,15-1,16))
        ((value (Grouping (Square_bracket Right))) (section file.txt:1,16-1,17))
        ((value (Symbol (Operator (Base (Div Div)))))
         (section file.txt:1,18-1,20))
        ((value (Identifier ((name d)))) (section file.txt:1,21-1,22))
        ((value (Symbol (Operator (Base (Pipe Greater)))))
         (section file.txt:1,23-1,25))
        ((value (Identifier ((name e)))) (section file.txt:1,26-1,27))
        ((value (Identifier ((name f)))) (section file.txt:1,28-1,29))
        ((value (Grouping (Parenthesis Right))) (section file.txt:1,29-1,30))
        ((value (Symbol (Operator (Base (Ampersand Ampersand)))))
         (section file.txt:1,31-1,33))
        ((value (Grouping (Parenthesis Left))) (section file.txt:1,34-1,35))
        ((value (Identifier ((name g)))) (section file.txt:1,35-1,36))
        ((value (Symbol (Operator (Base (Times Times)))))
         (section file.txt:1,37-1,39))
        ((value (Identifier ((name h)))) (section file.txt:1,40-1,41))
        ((value (Symbol (Operator (Base (Caret))))) (section file.txt:1,42-1,43))
        ((value (Identifier ((name i)))) (section file.txt:1,44-1,45))
        ((value (Symbol (Operator (Base (At))))) (section file.txt:1,46-1,47))
        ((value (Identifier ((name j)))) (section file.txt:1,48-1,49))
        ((value (Grouping (Parenthesis Right))) (section file.txt:1,49-1,50))
        ((value (Symbol (Operator (Base (Pipe Pipe)))))
         (section file.txt:1,51-1,53))
        ((value (Grouping (Parenthesis Left))) (section file.txt:1,54-1,55))
        ((value (Symbol (Operator (Base (Dollar)))))
         (section file.txt:1,55-1,56))
        ((value (Identifier ((name k)))) (section file.txt:1,56-1,57))
        ((value (Symbol (Operator (Base (Less Tilda Greater)))))
         (section file.txt:1,58-1,61))
        ((value (Identifier ((name l)))) (section file.txt:1,62-1,63))
        ((value (Symbol (Operator (Base (Percent)))))
         (section file.txt:1,63-1,64))
        ((value (Grouping (Parenthesis Right))) (section file.txt:1,64-1,65))
        ((value (Definition In)) (section file.txt:1,66-1,68))
        ((value (Identifier ((name bar)))) (section file.txt:2,0-2,3))
        ((value (Symbol (Operator (Base (Equal))))) (section file.txt:2,4-2,5))
        ((value (Grouping (Parenthesis Left))) (section file.txt:2,6-2,7))
        ((value (Identifier ((name m)))) (section file.txt:2,7-2,8))
        ((value (Symbol Comma)) (section file.txt:2,8-2,9))
        ((value (Identifier ((name n)))) (section file.txt:2,10-2,11))
        ((value (Grouping (Parenthesis Right))) (section file.txt:2,11-2,12))
        ((value (Symbol Walrus)) (section file.txt:2,13-2,15))
        ((value (Grouping (Square_bracket Left))) (section file.txt:2,16-2,17))
        ((value (Identifier ((name o)))) (section file.txt:2,17-2,18))
        ((value (Symbol (Operator (Base (Equal))))) (section file.txt:2,19-2,20))
        ((value (Identifier ((name p)))) (section file.txt:2,21-2,22))
        ((value (Symbol Semicolon)) (section file.txt:2,22-2,23))
        ((value (Identifier ((name q)))) (section file.txt:2,24-2,25))
        ((value (Symbol Colon)) (section file.txt:2,26-2,27))
        ((value (Identifier ((name r)))) (section file.txt:2,28-2,29))))
      (errors (("Failed to lex" (input .s))))))
    |}]
;;

let%expect_test "import" =
  let tokens =
    lex_pl
      {|
open! stdlib
open async
include import
|}
  in
  print_s
    [%message (tokens : Token_pl.t Source_position.With_section.t list With_errors.t)];
  [%expect
    {|
    (tokens
     ((value
       (((value (Import (Open (allow_unused true)))) (section file.txt:1,0-1,5))
        ((value (Identifier ((name stdlib)))) (section file.txt:1,6-1,12))
        ((value (Import (Open (allow_unused false)))) (section file.txt:2,0-2,4))
        ((value (Identifier ((name async)))) (section file.txt:2,5-2,10))
        ((value (Import Include)) (section file.txt:3,0-3,7))
        ((value (Identifier ((name import)))) (section file.txt:3,8-3,14))))
      (errors ())))
    |}]
;;

let%expect_test "definition" =
  let tokens =
    lex_pl
      {|
module#monad T : sig
   foo : ~a -> b -> c;
   bar : unit -> unit;
end = struct
   rec foo ~a b =
      #bind a and b in
      a + b
   and bar () = ();
end
|}
  in
  print_s
    [%message (tokens : Token_pl.t Source_position.With_section.t list With_errors.t)];
  [%expect
    {|
    (tokens
     ((value
       (((value (Definition (Module (((name monad))))))
         (section file.txt:1,0-1,12))
        ((value (Big_identifier ((name T)))) (section file.txt:1,13-1,14))
        ((value (Symbol Colon)) (section file.txt:1,15-1,16))
        ((value (Definition Sig)) (section file.txt:1,17-1,20))
        ((value (Identifier ((name foo)))) (section file.txt:2,3-2,6))
        ((value (Symbol Colon)) (section file.txt:2,7-2,8))
        ((value (Symbol (Operator (Base (Tilda))))) (section file.txt:2,9-2,10))
        ((value (Identifier ((name a)))) (section file.txt:2,10-2,11))
        ((value (Symbol (Operator (Base (Minus Greater)))))
         (section file.txt:2,12-2,14))
        ((value (Identifier ((name b)))) (section file.txt:2,15-2,16))
        ((value (Symbol (Operator (Base (Minus Greater)))))
         (section file.txt:2,17-2,19))
        ((value (Identifier ((name c)))) (section file.txt:2,20-2,21))
        ((value (Symbol Semicolon)) (section file.txt:2,21-2,22))
        ((value (Identifier ((name bar)))) (section file.txt:3,3-3,6))
        ((value (Symbol Colon)) (section file.txt:3,7-3,8))
        ((value (Identifier ((name unit)))) (section file.txt:3,9-3,13))
        ((value (Symbol (Operator (Base (Minus Greater)))))
         (section file.txt:3,14-3,16))
        ((value (Identifier ((name unit)))) (section file.txt:3,17-3,21))
        ((value (Symbol Semicolon)) (section file.txt:3,21-3,22))
        ((value (Definition End)) (section file.txt:4,0-4,3))
        ((value (Symbol (Operator (Base (Equal))))) (section file.txt:4,4-4,5))
        ((value (Definition Struct)) (section file.txt:4,6-4,12))
        ((value (Definition Rec)) (section file.txt:5,3-5,6))
        ((value (Identifier ((name foo)))) (section file.txt:5,7-5,10))
        ((value (Symbol (Operator (Base (Tilda))))) (section file.txt:5,11-5,12))
        ((value (Identifier ((name a)))) (section file.txt:5,12-5,13))
        ((value (Identifier ((name b)))) (section file.txt:5,14-5,15))
        ((value (Symbol (Operator (Base (Equal))))) (section file.txt:5,16-5,17))
        ((value (Definition (Assign_with_transformation ((name bind)))))
         (section file.txt:6,6-6,11))
        ((value (Identifier ((name a)))) (section file.txt:6,12-6,13))
        ((value (Definition And)) (section file.txt:6,14-6,17))
        ((value (Identifier ((name b)))) (section file.txt:6,18-6,19))
        ((value (Definition In)) (section file.txt:6,20-6,22))
        ((value (Identifier ((name a)))) (section file.txt:7,6-7,7))
        ((value (Symbol (Operator (Base (Plus))))) (section file.txt:7,8-7,9))
        ((value (Identifier ((name b)))) (section file.txt:7,10-7,11))
        ((value (Definition And)) (section file.txt:8,3-8,6))
        ((value (Identifier ((name bar)))) (section file.txt:8,7-8,10))
        ((value (Grouping (Parenthesis Left))) (section file.txt:8,11-8,12))
        ((value (Grouping (Parenthesis Right))) (section file.txt:8,12-8,13))
        ((value (Symbol (Operator (Base (Equal))))) (section file.txt:8,14-8,15))
        ((value (Grouping (Parenthesis Left))) (section file.txt:8,16-8,17))
        ((value (Grouping (Parenthesis Right))) (section file.txt:8,17-8,18))
        ((value (Symbol Semicolon)) (section file.txt:8,18-8,19))
        ((value (Definition End)) (section file.txt:9,0-9,3))))
      (errors ())))
    |}]
;;

let%expect_test "conditional" =
  let tokens =
    lex_pl
      {|
x = match foo:
    | Some bar when bar: 10
    | Some bar: 22
    | None: if true then 0 else 1 in
y = function
    | _ -> x;
|}
  in
  print_s
    [%message (tokens : Token_pl.t Source_position.With_section.t list With_errors.t)];
  [%expect
    {|
    (tokens
     ((value
       (((value (Identifier ((name x)))) (section file.txt:1,0-1,1))
        ((value (Symbol (Operator (Base (Equal))))) (section file.txt:1,2-1,3))
        ((value (Conditional (Match ()))) (section file.txt:1,4-1,9))
        ((value (Identifier ((name foo)))) (section file.txt:1,10-1,13))
        ((value (Symbol Colon)) (section file.txt:1,13-1,14))
        ((value (Symbol (Operator (Base (Pipe))))) (section file.txt:2,4-2,5))
        ((value (Big_identifier ((name Some)))) (section file.txt:2,6-2,10))
        ((value (Identifier ((name bar)))) (section file.txt:2,11-2,14))
        ((value (Conditional When)) (section file.txt:2,15-2,19))
        ((value (Identifier ((name bar)))) (section file.txt:2,20-2,23))
        ((value (Symbol Colon)) (section file.txt:2,23-2,24))
        ((value (Constant (Int 10))) (section file.txt:2,25-2,27))
        ((value (Symbol (Operator (Base (Pipe))))) (section file.txt:3,4-3,5))
        ((value (Big_identifier ((name Some)))) (section file.txt:3,6-3,10))
        ((value (Identifier ((name bar)))) (section file.txt:3,11-3,14))
        ((value (Symbol Colon)) (section file.txt:3,14-3,15))
        ((value (Constant (Int 22))) (section file.txt:3,16-3,18))
        ((value (Symbol (Operator (Base (Pipe))))) (section file.txt:4,4-4,5))
        ((value (Big_identifier ((name None)))) (section file.txt:4,6-4,10))
        ((value (Symbol Colon)) (section file.txt:4,10-4,11))
        ((value (Conditional (If ()))) (section file.txt:4,12-4,14))
        ((value (Constant (Bool true))) (section file.txt:4,15-4,19))
        ((value (Conditional Then)) (section file.txt:4,20-4,24))
        ((value (Constant (Int 0))) (section file.txt:4,25-4,26))
        ((value (Conditional Else)) (section file.txt:4,27-4,31))
        ((value (Constant (Int 1))) (section file.txt:4,32-4,33))
        ((value (Definition In)) (section file.txt:4,34-4,36))
        ((value (Identifier ((name y)))) (section file.txt:5,0-5,1))
        ((value (Symbol (Operator (Base (Equal))))) (section file.txt:5,2-5,3))
        ((value (Conditional Function)) (section file.txt:5,4-5,12))
        ((value (Symbol (Operator (Base (Pipe))))) (section file.txt:6,4-6,5))
        ((value (Definition Underscore)) (section file.txt:6,6-6,7))
        ((value (Symbol (Operator (Base (Minus Greater)))))
         (section file.txt:6,8-6,10))
        ((value (Identifier ((name x)))) (section file.txt:6,11-6,12))
        ((value (Symbol Semicolon)) (section file.txt:6,12-6,13))))
      (errors ())))
    |}]
;;

let%expect_test "typeful" =
  let tokens =
    lex_pl
      {|
type t = Foo of int
type nonrec t2 = { mutable foo : int }
x ({ foo } as bar) = foo;
|}
  in
  print_s
    [%message (tokens : Token_pl.t Source_position.With_section.t list With_errors.t)];
  [%expect
    {|
    (tokens
     ((value
       (((value (Typeful Type)) (section file.txt:1,0-1,4))
        ((value (Identifier ((name t)))) (section file.txt:1,5-1,6))
        ((value (Symbol (Operator (Base (Equal))))) (section file.txt:1,7-1,8))
        ((value (Big_identifier ((name Foo)))) (section file.txt:1,9-1,12))
        ((value (Typeful Of)) (section file.txt:1,13-1,15))
        ((value (Identifier ((name int)))) (section file.txt:1,16-1,19))
        ((value (Typeful Type)) (section file.txt:2,0-2,4))
        ((value (Typeful Nonrec)) (section file.txt:2,5-2,11))
        ((value (Identifier ((name t2)))) (section file.txt:2,12-2,14))
        ((value (Symbol (Operator (Base (Equal))))) (section file.txt:2,15-2,16))
        ((value (Grouping (Curly_bracket Left))) (section file.txt:2,17-2,18))
        ((value (Typeful Mutable)) (section file.txt:2,19-2,26))
        ((value (Identifier ((name foo)))) (section file.txt:2,27-2,30))
        ((value (Symbol Colon)) (section file.txt:2,31-2,32))
        ((value (Identifier ((name int)))) (section file.txt:2,33-2,36))
        ((value (Grouping (Curly_bracket Right))) (section file.txt:2,37-2,38))
        ((value (Identifier ((name x)))) (section file.txt:3,0-3,1))
        ((value (Grouping (Parenthesis Left))) (section file.txt:3,2-3,3))
        ((value (Grouping (Curly_bracket Left))) (section file.txt:3,3-3,4))
        ((value (Identifier ((name foo)))) (section file.txt:3,5-3,8))
        ((value (Grouping (Curly_bracket Right))) (section file.txt:3,9-3,10))
        ((value (Typeful As)) (section file.txt:3,11-3,13))
        ((value (Identifier ((name bar)))) (section file.txt:3,14-3,17))
        ((value (Grouping (Parenthesis Right))) (section file.txt:3,17-3,18))
        ((value (Symbol (Operator (Base (Equal))))) (section file.txt:3,19-3,20))
        ((value (Identifier ((name foo)))) (section file.txt:3,21-3,24))
        ((value (Symbol Semicolon)) (section file.txt:3,24-3,25))))
      (errors ())))
    |}]
;;

let%expect_test "misc" =
  let tokens =
    lex_pl
      {|
x = lambda a b c: a + b + c;

include functor Make with type t := t
|}
  in
  print_s
    [%message (tokens : Token_pl.t Source_position.With_section.t list With_errors.t)];
  [%expect
    {|
    (tokens
     ((value
       (((value (Identifier ((name x)))) (section file.txt:1,0-1,1))
        ((value (Symbol (Operator (Base (Equal))))) (section file.txt:1,2-1,3))
        ((value Lambda) (section file.txt:1,4-1,10))
        ((value (Identifier ((name a)))) (section file.txt:1,11-1,12))
        ((value (Identifier ((name b)))) (section file.txt:1,13-1,14))
        ((value (Identifier ((name c)))) (section file.txt:1,15-1,16))
        ((value (Symbol Colon)) (section file.txt:1,16-1,17))
        ((value (Identifier ((name a)))) (section file.txt:1,18-1,19))
        ((value (Symbol (Operator (Base (Plus))))) (section file.txt:1,20-1,21))
        ((value (Identifier ((name b)))) (section file.txt:1,22-1,23))
        ((value (Symbol (Operator (Base (Plus))))) (section file.txt:1,24-1,25))
        ((value (Identifier ((name c)))) (section file.txt:1,26-1,27))
        ((value (Symbol Semicolon)) (section file.txt:1,27-1,28))
        ((value (Import Include)) (section file.txt:3,0-3,7))
        ((value Functor) (section file.txt:3,8-3,15))
        ((value (Big_identifier ((name Make)))) (section file.txt:3,16-3,20))
        ((value With) (section file.txt:3,21-3,25))
        ((value (Typeful Type)) (section file.txt:3,26-3,30))
        ((value (Identifier ((name t)))) (section file.txt:3,31-3,32))
        ((value (Symbol Walrus)) (section file.txt:3,33-3,35))
        ((value (Identifier ((name t)))) (section file.txt:3,36-3,37))))
      (errors ())))
    |}]
;;
