open Higher

module Id = struct
  module M = Newtype1 (struct
    type 'a t = 'a
  end)

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
end

module Free = struct
  type ('self, _, _) u =
    | Pure : 'a -> ('self, 'a, 'f) u
    | Free : (('a, ('f, 'self) app) app, 'f) app -> ('self, 'a, 'f) u

  module rec M' : sig
    type ('a, 'f) s = (U.t, 'a, 'f) u
  end = struct
    type ('a, 'f) s = (U.t, 'a, 'f) u
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
  module Functor (C : Control.Functor.S) :
    Control.Functor.S' with type f = (C.f, M.t) app = Control.Functor.Wrap (struct
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
end
