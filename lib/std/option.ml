open struct
  module Option = Stdlib.Option
end

open Higher
module M = Newtype1 (Option)
include M

module Functor : Control.Functor.S' with type f = M.t = Control.Functor.Wrap (struct
  type f = M.t

  let fmap f m =
    let m' = M.prj m in
    Option.map f m' |> M.inj
  ;;
end)

module Applicative : Control.Applicative.S' with type f = M.t =
Control.Applicative.Wrap (struct
  module Functor = Functor
  include Functor

  let pure v = Option.some v |> M.inj

  let app fab fa =
    match M.prj fab with
    | Some ab -> Functor.fmap ab fa
    | None -> M.inj None
  ;;
end)

module Monad : Control.Monad.S' with type f = M.t = Control.Monad.Wrap (struct
  module Applicative = Applicative
  include Applicative

  let return = pure

  let bind m k =
    match M.prj m with
    | Some v -> k v
    | None -> M.inj None
  ;;
end)

let monad : (module Control.Monad.S) = (module Monad)
