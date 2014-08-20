open Monad

module StateT = functor (S : Type) -> functor (M : Monad) -> struct
  module Monad = struct
    type 'a t = S.t -> S.t * 'a M.t
    let return a = fun s -> (s, M.return a)
    let bind m f = fun s -> let (s', m') = m s in M.bind m' (fun a -> f a s')
  end
  open Monad

  let lift a = fun s -> (s, a)
end

let c = 0;;