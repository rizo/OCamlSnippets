open Monad

type ('a, 'b) either = Left of 'a | Right of 'b

module EitherT = functor (L : Type) -> functor (M : Monad) -> struct
  module Monad = struct
    type 'a t = ((L.t,'a) either) M.t
    let return a = M.return (Right a)
    let bind m f = M.bind m
      (fun e -> match e with
      | Left e  -> M.return (Left e)
      | Right a -> f a
      )
    let throwError e = M.return (Left e)
  end
  type 'a t = 'a Monad.t
  let return : 'a -> 'a t
             = Monad.return
  let bind : 'a t -> ('a -> 'b t) -> 'b t
           = Monad.bind
  let throwError : L.t -> 'a M.t
                 = Monad.throwError

  let lift (a : 'a M.t) : 'a t
                        = M.bind a (fun x -> M.return (Right x))
end

let c = 0;;