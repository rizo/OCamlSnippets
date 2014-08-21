open Monad

module ReaderT = functor (R : Type) -> functor (M : Monad) -> struct
  module Monad = struct
    type 'a t = R.t -> 'a M.t
    let return a  = fun _ -> M.return a
    let bind m f  = fun r -> M.bind (m r) (fun a -> f a r)
  end
  type 'a t = 'a Monad.t
  let return : 'a -> 'a t
             = Monad.return
  let bind : 'a t -> ('a -> 'b t) -> 'b t
           = Monad.bind
  let ask : R.t t
          = fun r -> M.return r
  let local (f : R.t -> R.t) (m : 'a t) : 'a t
                                        = fun r -> m (f r)
  
  let lift (a : 'a M.t) : 'a t = fun _ -> a
end

let c = 0;;