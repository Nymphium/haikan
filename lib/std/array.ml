open struct
  module Array = Stdlib.Array
end

open Higher
open Pre
module M = Newtype1 (Array)
include M

module Semigroup (C : RequiredType1) : Data.Semigroup.S' with type t = (C.p, M.t) app =
Data.Semigroup.Wrap (struct
  type t = (C.p, M.t) app

  let mconcat l r = M.inj @@ Array.append (M.prj l) (M.prj r)
end)

module Monoid (C : RequiredType1) : Data.Monoid.S with type t = (C.p, M.t) app = struct
  module Semigroup = Semigroup (C)
  include Semigroup

  let mempty = M.inj @@ Array.init 0 Obj.magic
end

module Foldable (C : RequiredType1) : Data.Foldable.S with type f = M.t = struct
  module Monoid = Monoid (C)
  include Monoid

  type f = M.t

  let foldr f init list =
    let l = M.prj list in
    Array.fold_right f l init
  ;;

  let rec fold_map m l =
    match M.prj l |> Array.to_list with
    | [] -> Monoid.mempty
    | x :: xs -> Monoid.mconcat (m x) @@ fold_map m @@ M.inj (Array.of_list xs)
  ;;
end

let monnoid' : type a. unit -> (module Data.Monoid.S with type t = (a, M.t) app) =
 fun () ->
  (module Monoid (struct
    type p = a
  end))
;;

module Functor : Control.Functor.S' with type f = M.t = Control.Functor.Wrap (struct
  type f = M.t

  let fmap f m =
    let m' = M.prj m in
    Array.map f m' |> M.inj
  ;;
end)

module Applicative : Control.Applicative.S' with type f = M.t =
Control.Applicative.Wrap (struct
  module Functor = Functor
  include Functor

  let pure v = [ v ] |> Array.of_list |> M.inj

  let rec app fab fa =
    match M.prj fab |> Array.to_list with
    | f :: fs ->
      let lhs = M.prj @@ Functor.fmap f fa in
      let rhs = M.prj @@ app (M.inj @@ Array.of_list fs) fa in
      M.inj @@ Array.append lhs rhs
    | [] -> M.inj @@ Array.init 0 Obj.magic
  ;;
end)

module Monad : Control.Monad.S' with type f = M.t = Control.Monad.Wrap (struct
  module Applicative = Applicative
  include Applicative

  let return = pure

  let rec bind m k =
    match M.prj m |> Array.to_list with
    | x :: xs ->
      let lhs = M.prj @@ k x in
      let rhs = M.prj @@ bind (M.inj @@ Array.of_list xs) k in
      M.inj @@ Array.append lhs rhs
    | [] -> M.inj @@ Array.init 0 Obj.magic
  ;;
end)

module Traverse (C : RequiredType1) : Data.Traverse.S with type f = M.t = struct
  module Foldable = Foldable (C)
  module Functor : Control.Functor.S with type f = Foldable.f = Functor
  include Foldable
  include Functor

  let sequence_a
      : type g.
        (module Control.Applicative.S with type f = g)
        -> (('a, g) app, f) app
        -> (('a, f) app, g) app
    =
   fun ap us ->
    let module Ap = (val ap) in
    let q u v =
      Ap.(
        app
          (fmap (fun h t -> M.inj @@ Array.of_list @@ h :: (Array.to_list @@ M.prj t)) u)
          v)
    in
    foldr q (Ap.pure @@ M.inj @@ Array.init 0 Obj.magic) us
 ;;

  let traverse
      : type g.
        (module Control.Applicative.S with type f = g)
        -> ('a -> ('b, g) app)
        -> ('a, f) app
        -> (('b, f) app, g) app
    =
   fun ap f xs ->
    let module Ap = (val ap) in
    let q x v =
      Ap.(
        app
          (fmap
             (fun h t -> M.inj @@ Array.of_list @@ h :: (Array.to_list @@ M.prj t))
             (f x))
          v)
    in
    foldr q (Ap.pure @@ M.inj @@ Array.init 0 Obj.magic) xs
 ;;
end
