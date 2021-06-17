open Higher

module M = Newtype1 (struct
  type 'a t = 'a
end)

include M

module Functor : Control.Functor.S' with type f = M.t = Control.Functor.Wrap (struct
  type f = M.t

  let fmap f m = M.inj @@ f @@ M.prj m
end)

module Applicative : Control.Applicative.S' with type f = M.t =
Control.Applicative.Wrap (struct
  module Functor = Functor
  include Functor

  let pure = M.inj
  let app fab fa = M.inj @@ (M.prj fab) (M.prj fa)
end)

module Monad : Control.Monad.S' with type f = M.t = Control.Monad.Wrap (struct
  module Applicative = Applicative
  include Applicative

  let return = pure
  let bind m k = k @@ M.prj m
end)

let monad : (module Control.Monad.S) = (module Monad)
