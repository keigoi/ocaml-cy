module type S = sig 
  type _ t
end
module type VAR = sig
  type t
end

module Cy(M:S)(Var:VAR) = struct 
  open M
  type cy = 
  | Cy : (Var.t -> cy t) -> cy
  | Cy2 : ((Var.t * Var.t -> cy t) * (Var.t * Var.t -> cy t)) -> cy
  | Var : Var.t -> cy
end


module Fold(C:S)(B:S)(Var:VAR) = struct

  module CyC = Cy(C)(Var)
  module CyB = Cy(B)(Var)

  type cyc = CyC.cy
  type cyb = CyB.cy

  type 'cyb cata = {cataf:'cyc. 'cyc C.t -> ('cyc -> 'cyb) -> 'cyb B.t}

  let rec fold : cyb cata -> cyc -> cyb = 
    fun (cata : cyb cata) (cy:cyc) -> 
      match cy with
      | Cy f -> 
        CyB.Cy (fun bx -> cata.cataf (f bx) (fun tl -> fold cata tl))
      | Cy2 f -> 
        CyB.Cy2 
          ((fun bx -> cata.cataf ((fst f bx)) (fold cata)),
           (fun bx -> cata.cataf ((snd f bx)) (fold cata)))
      | Var (v) -> Var(v)
end

module type S_print = sig
  include S
  val print : 'a t -> ('a -> string) -> string
end

module String = struct
  type t = string
end

module Print(C:S_print) = struct
  module CyC = Cy(C)(String)

  let rec print0 cnt : CyC.cy -> string = fun cy ->
    match cy with
    | Cy f -> 
        let svar = Printf.sprintf "x%d" cnt in
        Printf.sprintf "Cy(%s. %s)" svar 
        @@
        C.print 
          (f svar)
          (print0 (cnt+1))
    | Cy2 (f,g) -> 
        let svar1 = Printf.sprintf "x%d" cnt in
        let svar2 = Printf.sprintf "x%d" (cnt+1) in
        Printf.sprintf "Cy(%s,%s. <%s, %s>)" svar1 svar2 
        (C.print 
          (f (svar1,svar2))
          (print0 (cnt+2)))
        (C.print 
          (g (svar1,svar2))
          (print0 (cnt+2)))
          
    | Var v -> v

  let print = print0 0
end

module ListF = struct 
  type 'tl t = Cons of int * 'tl
  let print (Cons(x,xs)) f = Printf.sprintf "Cons(%d, %s)" x (f xs)
end

module BoolF = struct
  (* type _ t = True | False *)
end

module ListPrint = Print(ListF)

module Main(Var:VAR) = struct
  module ListCy = Cy(ListF)(Var)
  module ListFold = Fold(ListF)(ListF)(Var)

  let plus = {ListFold.cataf=(fun (Cons(x,xs)) fld -> Cons(x+1, fld xs))}

  let cyplus cy = ListFold.fold plus cy

  let ones = ListCy.Cy (fun x -> Cons (1, Var(x)))
  let mutual = 
    ListCy.Cy2 ((fun (_,y) -> Cons(1,Var y)), (fun (x,_) -> Cons(2,Var x)))
end

let test () =
  let module MainStr = Main(String) in
  print_endline (ListPrint.print MainStr.ones); 
  (* Cy(x0. Cons(1, x0)) *)
  print_endline (ListPrint.print (MainStr.cyplus MainStr.ones));
  (* Cy(x0. Cons(2, x0)) *)
  print_endline (ListPrint.print MainStr.mutual);
  (* Cy(x0,x1. <Cons(1, x1), Cons(2, x0)>) *)
  print_endline (ListPrint.print (MainStr.cyplus MainStr.mutual));
  (* Cy(x0,x1. <Cons(2, x1), Cons(3, x0)>) *)
  ()


(* module type S = sig 
  type _ t
end

module Cy(M:S) = struct 
  open M

  type (_,_,_,_,_) cyfuns =
    | Fun1 : 
      ('args -> 'r) -> 
        ('v, 'r, 'args, 'v, 'r) cyfuns
    | FunN : 
      ('args -> 'r) * ('vs, 'rs, 'args, 'v, 'r) cyfuns -> 
        ('v * 'vs, 'r * 'rs, 'args, 'v, 'r) cyfuns

  and _ cy = 
  | Cy : ('vs, 'rs, 'vs, 'v, 'v cy t) cyfuns -> 'v cy
  | Var : 'v -> 'v cy

end

module Fold(C:S)(B:S) = struct

  module CyC = Cy(C)
  module CyB = Cy(B)

  type 'v cyc = 'v CyC.cy
  type 'v cyb = 'v CyB.cy

  type 'cyb cata = {cataf:'cyc. 'cyc C.t -> ('cyc -> 'cyb) -> 'cyb B.t}

  (* let rec fold : type v. v cyb cata -> v cyc -> v cyb = 
    fun cata cy -> 
      match cy with
      | CyC.Cy fs -> 
        let calls 
          : type vs rs rs2 args. (vs,rs, args , v, v cyc C.t) CyC.cyfuns -> v cyb 
          = 
          function
          | CyC.Fun1 f -> 
              CyB.Cy (CyB.Fun1 (fun bx -> cata.cataf (f bx) (fold cata)))
          | _ -> failwith ""
        in
        calls fs
      | Var (v) -> Var(v) *)

  let rec fold : type v. v cyb cata -> v cyc -> v cyb = 
    fun cata cy -> 
      match cy with
      | CyC.Cy (Fun1 f)->
        CyB.Cy 
          (CyB.Fun1
            (fun bx ->
              cata.cataf 
                (f bx)
                (fold cata)))
      | CyC.Cy (FunN (f,fs)) -> 
          let calls 
            : type vs rs rs2 args. (vs,rs,args,v,v cyb C.t) CyC.cyfuns -> (vs,rs2(* *),args,v,v cyb B.t) CyB.cyfuns 
            = 
            function
            | CyC.Fun1 f -> 
                CyB.Fun1 (fun bx -> cata.cataf (f bx) (fold cata))
            | _ -> failwith ""
          in
          CyB.Cy 
            (CyB.FunN (
              (fun bx -> cata.cataf (f bx) ((fold cata))), 
              calls fs))
      | Var (v) -> Var(v)
end

module ListF = struct 
  type 'tl t = Cons of int * 'tl
  let print (Cons(x,xs)) f = Printf.sprintf "Cons(%d, %s)" x (f xs)
end

module CyList = Cy(ListF)


open CyList 
let mutual_cy = 
  Cy (FunN ((fun (_,y) -> Cons(1,Var y)), Fun1(fun (x,_) -> Cons(2,Var x))))
 *)
