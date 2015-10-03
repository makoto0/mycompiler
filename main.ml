open Syntax

let limit = ref 1000

(* 1-1 *)
let rec printspace depth=
  match depth with
  |0 -> ()
  |_ -> printspace (depth-1);
        Printf.printf "  "

let rec printsyntaxts l depth=
  match l with
  |[] -> ()
  |x::xs -> printsyntaxi x depth;printsyntaxts xs depth
and printidtypes l depth=
  match l with
  |[] -> ()
  |(x,_)::xs ->printspace depth;
               Printf.printf "VAR %s\n" x;
               printidtypes xs depth
and printsyntaxi p depth=
  printspace depth;
  match p with
  |Unit -> Printf.printf "UNIT\n"
  |Bool(b) -> Printf.printf "BOOL %b\n" b
  |Int(i) -> Printf.printf "INT %d\n" i
  |Float(f) -> Printf.printf "FLOAT %f\n" f
  |Not(e) -> Printf.printf "NOT\n";
             printsyntaxi e (depth+1)
  |Neg(e) -> Printf.printf "NEG\n";
             printsyntaxi e (depth+1)
  |Add(e1,e2) -> Printf.printf "ADD\n";
                 printsyntaxi e1 (depth+1);
		 printsyntaxi e2 (depth+1)
  |Sub(e1,e2) -> Printf.printf "SUB\n";
                 printsyntaxi e1 (depth+1);
		 printsyntaxi e2 (depth+1)
  |FNeg(e) -> Printf.printf "FNEG\n";
              printsyntaxi e (depth+1)
  |FAdd(e1,e2) -> Printf.printf "FADD\n";
                  printsyntaxi e1 (depth+1);
		  printsyntaxi e2 (depth+1)
  |FSub(e1,e2) -> Printf.printf "FSUB\n";
                  printsyntaxi e1 (depth+1);
	 	  printsyntaxi e2 (depth+1)
  |FMul(e1,e2) -> Printf.printf "FMUL\n";
                  printsyntaxi e1 (depth+1);
		  printsyntaxi e2 (depth+1)
  |FDiv(e1,e2) -> Printf.printf "FDIV\n";
                  printsyntaxi e1 (depth+1);
		  printsyntaxi e2 (depth+1)
  |Eq(e1,e2) -> Printf.printf "EQ\n";
                printsyntaxi e1 (depth+1);
		printsyntaxi e2 (depth+1)
  |LE(e1,e2) -> Printf.printf "LE\n";
                printsyntaxi e1 (depth+1);
		printsyntaxi e2 (depth+1)
  |If(e1,e2,e3) -> Printf.printf "IF\n";
                   printsyntaxi e1 (depth+1);
		   printsyntaxi e2 (depth+1);
		   printsyntaxi e3 (depth+1)
  |Let((e1,_),e2,e3) -> Printf.printf "LET\n";
                        Printf.printf "  VAR %s\n" e1;
			printsyntaxi e2 (depth+1);
			printsyntaxi e3 (depth+1);
  |Var(x) -> Printf.printf "VAR %s\n" x
  |LetRec({ name = (x, _); args = yts; body = e1 }, e2)->
    Printf.printf "LETREC\n";
    Printf.printf "  VAR %s\n" x;
    printidtypes yts (depth+2);
    printsyntaxi e1 (depth+1);
    printsyntaxi e2 (depth+1)
  |App(e1,e2) -> Printf.printf "APP\n";
                 printsyntaxi e1 (depth+1);
		 printsyntaxts e2 (depth+2)
  |Tuple(l) -> Printf.printf "TUPLE\n";
               printsyntaxts l (depth+1)
  |LetTuple(l,e1,e2) ->
    Printf.printf "LETTUPLE\n";
    printidtypes l (depth+1);
    printsyntaxi e1 (depth+1);
    printsyntaxi e2 (depth+1)
  |Array(e1,e2) ->
    Printf.printf "ARRAY\n";
    printsyntaxi e1 (depth+1);
    printsyntaxi e2 (depth+1)
  |Get(e1,e2) ->
    Printf.printf "GET\n";
    printsyntaxi e1 (depth+1);
    printsyntaxi e2 (depth+1)
  |Put(e1,e2,e3) ->
    Printf.printf "PUT\n";
    printsyntaxi e1 (depth+1);
    printsyntaxi e2 (depth+1);
    printsyntaxi e3 (depth+1)

let printsyntax p=printsyntaxi p 0
(* 1-1 *)

let rec iter n e = (* 最適化処理をくりかえす (caml2html: main_iter) *)
  Format.eprintf "iteration %d@." n;
  if n = 0 then e else
  let e' = Elim.f (ConstFold.f (Inline.f (Assoc.f (Beta.f e)))) in
  if e = e' then e else
  iter (n - 1) e'


let lexbuf outchan l = (* バッファをコンパイルしてチャンネルへ出力する (caml2html: main_lexbuf) *)
  Id.counter := 0;
  Typing.extenv := M.empty;
  let p=(Parser.exp Lexer.token l) in
  let n= (KNormal.f (Typing.f p)) in
  Printf.printf "Syntax.t:\n";
  printsyntax p;
  (*Printf.printf "Normal.t:\n";
  printnormal n;*)
  Emit.f outchan
    (RegAlloc.f
       (Simm.f
	  (Virtual.f
	     (Closure.f
		(iter !limit
		   (Alpha.f n))))))

let string s = lexbuf stdout (Lexing.from_string s) (* 文字列をコンパイルして標準出力に表示する (caml2html: main_string) *)

let file f = (* ファイルをコンパイルしてファイルに出力する (caml2html: main_file) *)
  let inchan = open_in (f ^ ".ml") in
  let outchan = open_out (f ^ ".s") in
  try
    lexbuf outchan (Lexing.from_channel inchan);
    close_in inchan;
    close_out outchan;
  with e -> (close_in inchan; close_out outchan; raise e)

let () = (* ここからコンパイラの実行が開始される (caml2html: main_entry) *)
  let files = ref [] in
  Arg.parse
    [("-inline", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
     ("-iter", Arg.Int(fun i -> limit := i), "maximum number of optimizations iterated")]
    (fun s -> files := !files @ [s])
    ("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
     Printf.sprintf "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..." Sys.argv.(0));
  List.iter
    (fun f -> ignore (file f))
    !files

