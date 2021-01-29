
open Lam  
let [@inline] unknown _ x = x
let [@inline] option sub self = fun v -> 
  match v with 
  | None -> None
  | Some v -> Some (sub self v)
let rec list sub self = fun x  -> 
  match x with 
  | [] -> []
  | x::xs -> 
    let v = sub self x in 
    v :: list sub self xs

type iter = {
t : t fn
}  [@@ocaml.unboxed]
and 'a fn = iter -> 'a -> 'a
 let lambda_switch : lambda_switch fn  = fun _self { sw_consts_full = _x0;sw_consts = _x1;sw_blocks_full = _x2;sw_blocks = _x3;sw_failaction = _x4;sw_names = _x5} -> begin let _x1 = list ((fun _self (_x0,_x1) -> begin let _x1 = _self.t _self _x1 in  (_x0,_x1) end)) _self _x1 in 
let _x3 = list ((fun _self (_x0,_x1) -> begin let _x1 = _self.t _self _x1 in  (_x0,_x1) end)) _self _x3 in 
let _x4 = option _self.t _self _x4 in  {sw_consts_full = _x0;sw_consts = _x1;sw_blocks_full = _x2;sw_blocks = _x3;sw_failaction = _x4;sw_names = _x5} end 
 let lfunction : lfunction fn  = fun _self { arity = _x0;params = _x1;body = _x2;attr = _x3} -> begin let _x2 = _self.t _self _x2 in  {arity = _x0;params = _x1;body = _x2;attr = _x3} end 
 let prim_info : prim_info fn  = fun _self { primitive = _x0;args = _x1;loc = _x2} -> begin let _x1 = list _self.t _self _x1 in  {primitive = _x0;args = _x1;loc = _x2} end 
 let apply : apply fn  = fun _self { ap_func = _x0;ap_args = _x1;ap_info = _x2} -> begin let _x0 = _self.t _self _x0 in 
let _x1 = list _self.t _self _x1 in  {ap_func = _x0;ap_args = _x1;ap_info = _x2} end 
 let t : t fn  = fun _self -> function 
| Lvar _ as v -> v
|Lglobal_module _ as v -> v
|Lconst _ as v -> v
|Lapply ( _x0)  -> 
 begin let _x0 = apply _self _x0 in  Lapply ( _x0)  end
|Lfunction ( _x0)  -> 
 begin let _x0 = lfunction _self _x0 in  Lfunction ( _x0)  end
|Llet ( _x0,_x1,_x2,_x3)  -> 
 begin let _x2 = _self.t _self _x2 in 
let _x3 = _self.t _self _x3 in  Llet ( _x0,_x1,_x2,_x3)  end
|Lletrec ( _x0,_x1)  -> 
 begin let _x0 = list ((fun _self (_x0,_x1) -> begin let _x1 = _self.t _self _x1 in  (_x0,_x1) end)) _self _x0 in 
let _x1 = _self.t _self _x1 in  Lletrec ( _x0,_x1)  end
|Lprim ( _x0)  -> 
 begin let _x0 = prim_info _self _x0 in  Lprim ( _x0)  end
|Lswitch ( _x0,_x1)  -> 
 begin let _x0 = _self.t _self _x0 in 
let _x1 = lambda_switch _self _x1 in  Lswitch ( _x0,_x1)  end
|Lstringswitch ( _x0,_x1,_x2)  -> 
 begin let _x0 = _self.t _self _x0 in 
let _x1 = list ((fun _self (_x0,_x1) -> begin let _x1 = _self.t _self _x1 in  (_x0,_x1) end)) _self _x1 in 
let _x2 = option _self.t _self _x2 in  Lstringswitch ( _x0,_x1,_x2)  end
|Lstaticraise ( _x0,_x1)  -> 
 begin let _x1 = list _self.t _self _x1 in  Lstaticraise ( _x0,_x1)  end
|Lstaticcatch ( _x0,_x1,_x2)  -> 
 begin let _x0 = _self.t _self _x0 in 
let _x1 = (fun (_x0,_x1) -> begin  (_x0,_x1) end) _x1 in
let _x2 = _self.t _self _x2 in  Lstaticcatch ( _x0,_x1,_x2)  end
|Ltrywith ( _x0,_x1,_x2)  -> 
 begin let _x0 = _self.t _self _x0 in 
let _x2 = _self.t _self _x2 in  Ltrywith ( _x0,_x1,_x2)  end
|Lifthenelse ( _x0,_x1,_x2)  -> 
 begin let _x0 = _self.t _self _x0 in 
let _x1 = _self.t _self _x1 in 
let _x2 = _self.t _self _x2 in  Lifthenelse ( _x0,_x1,_x2)  end
|Lsequence ( _x0,_x1)  -> 
 begin let _x0 = _self.t _self _x0 in 
let _x1 = _self.t _self _x1 in  Lsequence ( _x0,_x1)  end
|Lwhile ( _x0,_x1)  -> 
 begin let _x0 = _self.t _self _x0 in 
let _x1 = _self.t _self _x1 in  Lwhile ( _x0,_x1)  end
|Lfor ( _x0,_x1,_x2,_x3,_x4)  -> 
 begin let _x1 = _self.t _self _x1 in 
let _x2 = _self.t _self _x2 in 
let _x4 = _self.t _self _x4 in  Lfor ( _x0,_x1,_x2,_x3,_x4)  end
|Lassign ( _x0,_x1)  -> 
 begin let _x1 = _self.t _self _x1 in  Lassign ( _x0,_x1)  end
|Lsend ( _x0,_x1,_x2,_x3,_x4)  -> 
 begin let _x1 = _self.t _self _x1 in 
let _x2 = _self.t _self _x2 in 
let _x3 = list _self.t _self _x3 in  Lsend ( _x0,_x1,_x2,_x3,_x4)  end 
let super : iter = {
t
}
