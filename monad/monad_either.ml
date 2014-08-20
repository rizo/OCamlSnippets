open Monad

type ('a, 'b) either = Left of 'a | Right of 'b

module EitherT = functor (L : Type) -> functor (M : Monad) -> struct
  module Monad = struct
    type 'a t = ((L.t,'a) either) M.t
    let return a = M.return (Right a)
    let bind m f = M.bind m
      (fun e -> match e with
      | Left e  -> return (Left e)
      | Right a -> f a
      )
  end
  open Monad

  let lift a = M.bind a (fun x -> return (Right x))
end

let c = 0;;