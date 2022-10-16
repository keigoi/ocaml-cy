
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
