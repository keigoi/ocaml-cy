module type S = sig 
  type _ t
end

module Cy(M:S) = struct 
  open M
  (* cy in PHOAS form *)
  type _ cy = 
  | Cy : ('v -> ('v cy) t) -> 'v cy
  | Cy2 : (('v * 'v -> 'v cy t) * ('v * 'v -> 'v cy t)) -> 'v cy
  | Var : 'v -> 'v cy
end

module Fold(C:S)(B:S) = struct

  module CyC = Cy(C)
  module CyB = Cy(B)

  type 'v cyc = 'v CyC.cy
  type 'v cyb = 'v CyB.cy

  type 'cyb cata = {cataf:'cyc. 'cyc C.t -> ('cyc -> 'cyb) -> 'cyb B.t}

  let rec fold : 'v . 'v cyb cata -> 'v cyc -> 'v cyb = 
    fun (type v) (cata : v cyb cata) (cy:v cyc) -> 
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

module Print(C:S_print) = struct
  module CyC = Cy(C)

  let rec print0 cnt : string CyC.cy -> string = fun cy ->
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

module ListCy = Cy(ListF)
module ListFold = Fold(ListF)(ListF)
module ListPrint = Print(ListF)

let plus = {ListFold.cataf=(fun (Cons(x,xs)) fld -> Cons(x+1, fld xs))}

let cyplus cy = ListFold.fold plus cy

let ones = ListCy.Cy (fun x -> Cons (1, Var(x)))
let mutual = 
  ListCy.Cy2 ((fun (_,y) -> Cons(1,Var y)), (fun (x,_) -> Cons(2,Var x)))

let test () =
  print_endline (ListPrint.print ones); 
  (* Cy(x0. Cons(1, x0)) *)
  print_endline (ListPrint.print (cyplus ones));
  (* Cy(x0. Cons(2, x0)) *)
  print_endline (ListPrint.print mutual);
  (* Cy(x0,x1. <Cons(1, x1), Cons(2, x0)>) *)
  print_endline (ListPrint.print (cyplus mutual));
  (* Cy(x0,x1. <Cons(2, x1), Cons(3, x0)>) *)
  ()
