open struct
  module Result = Stdlib.Result
end

open Higher
open Pre
module M = Newtype2 (Result)
include M

module Functor (C : RequiredType1) : Control.Functor.S' with type f = (C.p, M.t) app =
Control.Functor.Wrap (struct
  type f = (C.p, M.t) app

  let fmap f m =
    let m' = M.prj m in
    Result.map f m' |> M.inj
  ;;
end)

module Applicative (C : RequiredType1) :
  Control.Applicative.S' with type f = (C.p, M.t) app = Control.Applicative.Wrap (struct
  module Functor = Functor (C)
  include Functor

  let pure v = Result.ok v |> M.inj

  let app fab fa =
    match M.prj fab with
    | Ok ab -> Functor.fmap ab fa
    | Error e -> M.inj @@ Error e
  ;;
end)

module Monad (C : RequiredType1) : Control.Monad.S' with type f = (C.p, M.t) app =
Control.Monad.Wrap (struct
  module Applicative = Applicative (C)
  include Applicative

  let return = pure

  let bind m k =
    match M.prj m with
    | Ok v -> k v
    | Error e -> M.inj @@ Error e
  ;;
end)

(* unit for relaxing the value restriction *)
let monad' : type a. unit -> (module Control.Monad.S with type f = (a, M.t) app) =
 fun () ->
  (module Monad (struct
    type p = a
  end))
;;
