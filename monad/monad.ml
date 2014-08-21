module type Type = sig
  type t
end

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t 
end

module IdentityMonad = struct
  type 'a t = 'a
  let return a = a
  let bind a f = f a
end

module type MonadTrans = functor (M : Monad) -> sig
  module Monad : Monad
  val lift : 'a M.t -> 'a Monad.t
end

let c = 0;;