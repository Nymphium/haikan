open Higher

module rec M' : sig
  type (_, _) s =
    | Pure : 'a -> ('a, 'f) s
    | Free : (('a, ('f, U.t) app) app, 'f) app -> ('a, 'f) s
end = struct
  type (_, _) s =
    | Pure : 'a -> ('a, 'f) s
    | Free : (('a, ('f, U.t) app) app, 'f) app -> ('a, 'f) s
end

and U : (Newtype2 with type ('a, 'b) s = ('a, 'b) M'.s) = Newtype2 (struct
  type ('a, 'b) t = ('a, 'b) M'.s
end)

module M = struct
  include U
  include M'
end

let pure v = M.inj @@ Pure v
let free fv = M.inj @@ Free fv

(* perform :: (v -> (r -> Free f r) -> f (Free f a)) -> v -> Free f a *)
let perform op v = free @@ op v pure

let rec interpret' : (('free -> 'a) -> ('free, 'f) app -> 'a) -> 'free -> 'a =
 fun it m ->
  let interpret = it (interpret' it) in
  match M.prj m with
  | Free fv -> interpret fv
  | Pure v -> v
;;

(* Functor C.f => Functor (Free C.f) *)
module Functor (C : Control.Functor.S) : Control.Functor.S' with type f = (C.f, M.t) app =
Control.Functor.Wrap (struct
  type f = (C.f, M.t) app

  let rec fmap f m =
    match M.prj m with
    | Pure v -> pure (f v)
    | Free fv ->
      let f' = fmap f in
      free @@ C.fmap f' fv
  ;;
end)

module Applicative (C : Control.Functor.S) :
  Control.Applicative.S' with type f = (C.f, M.t) app = Control.Applicative.Wrap (struct
  module Functor = Functor (C)
  include Functor

  let pure = pure

  let rec app fab fa =
    match M.prj fab with
    | Pure f -> fmap f fa
    | Free fv -> free (C.fmap (fun g -> app g fa) fv)
  ;;
end)

module Monad (C : Control.Functor.S) : Control.Monad.S' with type f = (C.f, M.t) app =
Control.Monad.Wrap (struct
  module Applicative = Applicative (C)
  include Applicative

  let return = Applicative.pure

  let rec bind : ('a, f) app -> ('a -> ('b, f) app) -> ('b, f) app =
   fun m k ->
    match M.prj m with
    | Pure v -> k v
    | Free fv -> free (C.fmap (fun m' -> bind m' k) fv)
 ;;
end)

let monad
    : type g.
      (module Control.Functor.S with type f = g)
      -> (module Control.Monad.S with type f = (g, M.t) app)
  =
 fun fnctor ->
  let module F = (val fnctor) in
  (module Monad (F))
;;
