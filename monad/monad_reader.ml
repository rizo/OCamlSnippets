open Monad

module ReaderT = functor (R : Type) -> functor (M : Monad) -> struct
  module Monad = struct
    type 'a t = R.t -> 'a M.t
    let return a = fun _ -> M.return a
    let bind m f = fun r -> M.bind (m r) (fun a -> f a r)
  end
  open Monad
  
  let lift a = fun _ -> a
end

let c = 0;;