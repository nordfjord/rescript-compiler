
open Lam
let unknown _ _ = ()
let [@inline] option sub self = fun v -> 
  match v with 
  | None -> ()
  | Some v -> sub self v
let rec list sub self = fun x  -> 
  match x with 
  | [] -> ()
  | x::xs -> 
    sub self x ;
    list sub self xs

type iter = {
t : t fn
}  
and 'a fn = iter -> 'a -> unit
let  lambda_switch : lambda_switch fn  =  fun _self { sw_consts_full = _x0;sw_consts = _x1;sw_blocks_full = _x2;sw_blocks = _x3;sw_failaction = _x4;sw_names = _x5} -> begin list ((fun _self (_x0,_x1) -> begin _self.t _self _x1 end)) _self _x1;list ((fun _self (_x0,_x1) -> begin _self.t _self _x1 end)) _self _x3;option _self.t _self _x4 end   
let  lfunction : lfunction fn  =  fun _self { arity = _x0;params = _x1;body = _x2;attr = _x3} -> begin _self.t _self _x2 end   
let  prim_info : prim_info fn  =  fun _self { primitive = _x0;args = _x1;loc = _x2} -> begin list _self.t _self _x1 end   
let  apply : apply fn  =  fun _self { ap_func = _x0;ap_args = _x1;ap_info = _x2} -> begin _self.t _self _x0;list _self.t _self _x1 end   
let  t : t fn  =  fun _self -> function 
| Lvar _ -> ()
|Lglobal_module _ -> ()
|Lconst _ -> ()
|Lapply ( _x0)  -> 
 begin apply _self _x0 end
|Lfunction ( _x0)  -> 
 begin lfunction _self _x0 end
|Llet ( _x0,_x1,_x2,_x3)  -> 
 begin _self.t _self _x2;_self.t _self _x3 end
|Lletrec ( _x0,_x1)  -> 
 begin list ((fun _self (_x0,_x1) -> begin _self.t _self _x1 end)) _self _x0;_self.t _self _x1 end
|Lprim ( _x0)  -> 
 begin prim_info _self _x0 end
|Lswitch ( _x0,_x1)  -> 
 begin _self.t _self _x0;lambda_switch _self _x1 end
|Lstringswitch ( _x0,_x1,_x2)  -> 
 begin _self.t _self _x0;list ((fun _self (_x0,_x1) -> begin _self.t _self _x1 end)) _self _x1;option _self.t _self _x2 end
|Lstaticraise ( _x0,_x1)  -> 
 begin list _self.t _self _x1 end
|Lstaticcatch ( _x0,_x1,_x2)  -> 
 begin _self.t _self _x0;(fun (_x0,_x1) -> begin  end) _x1;_self.t _self _x2 end
|Ltrywith ( _x0,_x1,_x2)  -> 
 begin _self.t _self _x0;_self.t _self _x2 end
|Lifthenelse ( _x0,_x1,_x2)  -> 
 begin _self.t _self _x0;_self.t _self _x1;_self.t _self _x2 end
|Lsequence ( _x0,_x1)  -> 
 begin _self.t _self _x0;_self.t _self _x1 end
|Lwhile ( _x0,_x1)  -> 
 begin _self.t _self _x0;_self.t _self _x1 end
|Lfor ( _x0,_x1,_x2,_x3,_x4)  -> 
 begin _self.t _self _x1;_self.t _self _x2;_self.t _self _x4 end
|Lassign ( _x0,_x1)  -> 
 begin _self.t _self _x1 end
|Lsend ( _x0,_x1,_x2,_x3,_x4)  -> 
 begin _self.t _self _x1;_self.t _self _x2;list _self.t _self _x3 end   
let super : iter = {
t    
    }
    