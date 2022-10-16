module type S = sig 
  type _ t
end

module type SFold = sig
  include S
  type ('r,'s) cata
  val map : ('a -> 'b) -> 'a t -> 'b t
  val fold : ('r,'s) cata -> ('a -> 'r) -> 'a t -> 's
end

module ListF = struct 
  type 'tl t = Cons of int * 'tl

  type ('r,'rt) cata = {cata_cons:int -> 'r -> 'rt}

  let map f (Cons(x,xs)) = Cons(x, f xs)

  let fold : 'a 'r 'rt. ('r,'rt) cata -> ('a -> 'r) -> 'a t -> 'rt = 
    fun cata self (Cons(x,xs)) ->
    cata.cata_cons x (self xs)

  let print (Cons(x,xs)) f = Printf.sprintf "Cons(%d, %s)" x (f xs)
end
let cata_ex =
  {ListF.cata_cons = (fun x r -> x + r)}

module Cy(M:S) = struct 
  open M
  (* cy in PHOAS form *)
  type _ cy = 
  | Cy : ('v -> ('v cy) t) -> 'v cy
  | Var : 'v -> 'v cy
end

module Cy2(M:S)(N:S) = struct
  (* cy in PHOAS form *)
  type (_,_) cy2 = 
  | Cy2 : (('v * 'w -> ('v,'w) cy2 M.t) * ('v * 'w -> ('v,'w) cy2 N.t)) -> ('v,'w) cy2
  | Var1 : 'v -> ('v,'w) cy2
  | Var2 : 'w -> ('v,'w) cy2
end

module CyFold(C:SFold)(B:S) = struct

  module CyC = Cy(C)
  module CyB = Cy(B)

  type 'v cyc = 'v CyC.cy
  type 'v cyb = 'v CyB.cy

  let rec fold : 'v. ('v cyb, 'v cyb B.t) C.cata -> 'v cyc -> 'v cyb = 
    fun (type v) (cata : (v cyb, v cyb B.t) C.cata) (cy:v cyc) -> 
      match cy with
      | Cy f ->
        CyB.Cy (fun bx -> C.fold cata (fold cata) (f bx))
      | Var (v) -> Var(v)
end

module Cy2Fold(C1:SFold)(C2:SFold)(B1:SFold)(B2:SFold) = struct

  module CyC = Cy2(C1)(C2)
  module CyB = Cy2(B1)(B2)
  module CyC1 = Cy(C1)
  module CyC2 = Cy(C2)
  module CyB1 = Cy(B1)
  module CyB2 = Cy(B2)
  module Fold1 = CyFold(C1)(B1)
  module Fold2 = CyFold(C2)(B2)

  type ('v,'w) cyc = ('v,'w) CyC.cy2
  type ('v,'w) cyb = ('v,'w) CyB.cy2
  type 'v cyb1 = 'v CyB1.cy
  type 'v cyb2 = 'v CyB2.cy

  let rec bekic : 'v 'w. ('v,'w cyb2) cyb -> 'v cyb1 * 'w cyb2 =
    let fst_ y = B1.map (fun x -> fst @@ bekic x) y in
    let snd_ y = B2.map (fun x -> snd @@ bekic x) y in
    fun (type v w) (cyb : (v,w cyb2) cyb) ->
      match cyb with
      | Cy2 (t,s) -> 
        (CyB1.Cy (fun x -> fst_ @@ t (x, CyB2.Cy (fun y -> snd_ @@ s (x,y)))), 
        failwith "")
        (* let f1 x = 
          B1.map (fun x -> fst @@ bekic x) @@ 
          t (x,
            let g2 y = B2.map (fun x -> snd @@ bekic x) @@ g (x,y) in 
            CyB2.Cy g2
            )
        in
        (CyB1.Cy f1, failwith "abc") *)
      | Var1 _ -> failwith ""
      | Var2 _ -> failwith ""

  let rec fold : 'v 'w. 
    ('v cyb1 B1.t * 'w cyb2 B2.t, ('v,'w) cyb B1.t) C1.cata -> 
    ('v cyb1 B1.t * 'w cyb2 B2.t, ('v,'w) cyb B2.t) C2.cata -> 
    ('v,'w) cyc -> ('v,'w) cyb = 
    fun (type v w) 
      (cata1 : (v cyb1 B1.t * w cyb2 B2.t, (v,w) cyb B1.t) C1.cata)
      (cata2 : (v cyb1 B1.t * w cyb2 B2.t, (v,w) cyb B2.t) C2.cata)
      (cy:(v,w) cyc) -> 
      match cy with
      | Cy2 (f,g) ->
        CyB.Cy2 
          ((fun bx -> C1.fold cata1 (fun c -> bekic @@ fold cata1 cata2 c) (f bx)),
           (fun bx -> C2.fold cata2 (fun c -> bekic @@ fold cata1 cata2 c) (g bx)))
      | Var1 (v) -> Var1(v)
      | Var2 (v) -> Var2(v)
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
    | Var v -> v

  let print = print0 0
end

module Print2(C1:S_print)(C2:S_print) = struct
  module CyC = Cy2(C1)(C2)

  let rec print0 cnt : (string,string) CyC.cy2 -> string = fun cy ->
    match cy with
    | Cy2 (f,g) -> 
        let svar1 = Printf.sprintf "x%d" cnt in
        let svar2 = Printf.sprintf "x%d" (cnt+1) in
        Printf.sprintf "Cy(%s,%s. <%s, %s>)" svar1 svar2 
        (C1.print 
          (f (svar1,svar2))
          (print0 (cnt+2)))
        (C2.print 
          (g (svar1,svar2))
          (print0 (cnt+2)))
    | Var1 v -> v
    | Var2 v -> v

  let print = print0 0
end

module ListCy = Cy(ListF)
module ListPrint = Print(ListF)
module ListCy2 = Cy2(ListF)(ListF)
module ListPrint2 = Print2(ListF)(ListF)

let plus = ListF.{cata_cons=(fun x xs -> Cons(x+1, xs))}

module F = CyFold(ListF)(ListF)
module F2 = Cy2Fold(ListF)(ListF)(ListF)(ListF)

let cyplus cy = F.fold plus cy

let ones = ListCy.Cy (fun x -> Cons (1, Var(x)))
let mutual = 
  ListCy2.Cy2 ((fun (_,y) -> Cons(1, Var2 y)), (fun (x,_) -> Cons(2,Var1 x)))

let tail1 = 
  F2.fold
    ListF.{cata_cons=(fun x (_,y) -> y)}

let test () =
  print_endline (ListPrint.print ones); 
  (* Cy(x0. Cons(1, x0)) *)
  print_endline (ListPrint.print (cyplus ones));
  (* Cy(x0. Cons(2, x0)) *)
  print_endline (ListPrint2.print mutual);
  (* Cy(x0,x1. <Cons(1, x1), Cons(2, x0)>) *)
  print_endline (ListPrint2.print (F2.fold plus plus mutual));
  (* Cy(x0,x1. <Cons(2, x1), Cons(3, x0)>) *)
  ()
