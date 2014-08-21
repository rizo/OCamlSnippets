open Monad

module StateT = functor (S : Type) -> functor (M : Monad) -> struct
  module Monad = struct
    type 'a t = S.t -> (S.t * 'a) M.t
    let return a = fun s -> M.return (s, a)
    let bind (m : 'a t) (f : 'a -> 'b t) = fun s -> M.bind (m s) (fun (s', a) -> f a s')
  end
  type 'a t = S.t -> (S.t * 'a) M.t
  let return : 'a -> 'a t 
             = Monad.return 
  let bind : 'a t -> ('a -> 'b t) -> 'b t
           = Monad.bind
  let get : S.t t
          = fun s -> M.return (s, s)
  let put s : unit t
            = fun _ -> M.return (s, ())
  let modify f : unit t
               = fun s -> M.return (f s, ())

  let lift (a : 'a M.t) : 'a t
                        = fun s -> M.bind a (fun x -> M.return (s, x))
end

let c = 0;;