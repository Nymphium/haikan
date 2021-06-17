open Higher
open Pre

module Option = struct
  module M = Newtype1 (Option)

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
end

module List = struct
  module M = Newtype1 (List)

  module Functor : Control.Functor.S' with type f = M.t = Control.Functor.Wrap (struct
    type f = M.t

    let fmap f m =
      let m' = M.prj m in
      List.map f m' |> M.inj
    ;;
  end)

  module Applicative : Control.Applicative.S' with type f = M.t =
  Control.Applicative.Wrap (struct
    module Functor = Functor
    include Functor

    let pure v = [ v ] |> M.inj

    let rec app fab fa =
      match M.prj fab with
      | f :: fs ->
        let lhs = M.prj @@ Functor.fmap f fa in
        let rhs = M.prj @@ app (M.inj fs) fa in
        M.inj @@ lhs @ rhs
      | [] -> M.inj []
    ;;
  end)

  module Monad : Control.Monad.S' with type f = M.t = Control.Monad.Wrap (struct
    module Applicative = Applicative
    include Applicative

    let return = pure

    let rec bind m k =
      match M.prj m with
      | x :: xs ->
        let lhs = M.prj @@ k x in
        let rhs = M.prj @@ bind (M.inj xs) k in
        M.inj (lhs @ rhs)
      | [] -> M.inj []
    ;;
  end)

  module Semigroup (C : RequiredType1) : Data.Semigroup.S' with type t = (C.p, M.t) app =
  Data.Semigroup.Wrap (struct
    type t = (C.p, M.t) app

    let mconcat lhs rhs =
      let lhs = M.prj lhs in
      let rhs = M.prj rhs in
      M.inj @@ lhs @ rhs
    ;;
  end)

  module Monoid (C : RequiredType1) : Data.Monoid.S = struct
    module Semigroup = Semigroup (C)
    include Semigroup

    let mempty = M.inj []
  end

  module Foldable (C : RequiredType1) : Data.Foldable.S with type f = M.t = struct
    module Monoid = Monoid (C)
    include Monoid

    type f = M.t

    let foldr f init list =
      let l = M.prj list in
      List.fold_right f l init
    ;;

    let rec fold_map m l =
      match M.prj l with
      | [] -> Monoid.mempty
      | x :: xs -> Monoid.mconcat (m x) @@ fold_map m @@ M.inj xs
    ;;
  end

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
      let q u v = Ap.(app (fmap (fun h t -> M.inj @@ h :: M.prj t) u) v) in
      foldr q (Ap.pure @@ M.inj []) us
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
      let q x v = Ap.(app (fmap (fun h t -> M.inj @@ h :: M.prj t) (f x)) v) in
      foldr q (Ap.pure @@ M.inj []) xs
   ;;
  end

  let monad : (module Control.Monad.S with type f = M.t) = (module Monad)

  let traverse : type a. (module Data.Traverse.S with type f = M.t) =
    (module Traverse (struct
      type p = a
    end))
  ;;
end

module Result = struct
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
end

module Either = struct
  module M = Newtype2 (struct
    type ('a, 'b) t = ('b, 'a) Either.t
  end)

  include M

  module Functor (C : RequiredType1) : Control.Functor.S' with type f = (C.p, M.t) app =
  Control.Functor.Wrap (struct
    type f = (C.p, M.t) app

    let fmap f m =
      let m' = M.prj m in
      Either.map_right f m' |> M.inj
    ;;
  end)

  module Applicative (C : RequiredType1) :
    Control.Applicative.S' with type f = (C.p, M.t) app = Control.Applicative.Wrap (struct
    module Functor = Functor (C)
    include Functor

    let pure v = Either.right v |> M.inj

    let app fab fa =
      match M.prj fab with
      | Right ab -> Functor.fmap ab fa
      | Left e -> M.inj @@ Left e
    ;;
  end)

  module Monad (C : RequiredType1) : Control.Monad.S' with type f = (C.p, M.t) app =
  Control.Monad.Wrap (struct
    module Applicative = Applicative (C)
    include Applicative

    let return = pure

    let bind m k =
      match M.prj m with
      | Right v -> k v
      | Left e -> M.inj @@ Left e
    ;;
  end)

  (* unit for relaxing the value restriction *)
  let monad' : type a. unit -> (module Control.Monad.S with type f = (a, M.t) app) =
   fun () ->
    (module Monad (struct
      type p = a
    end))
 ;;
end

module String = struct
  module M = Newtype0 (String)

  module Semigroup : Data.Semigroup.S' with type t = M.t = Data.Semigroup.Wrap (struct
    type t = M.t

    let mconcat l r = M.inj @@ M.prj l ^ M.prj r
  end)

  module Monoid : Data.Monoid.S with type t = M.t = struct
    module Semigroup = Semigroup
    include Semigroup

    let mempty = M.inj ""
  end
end

module Array = struct
  module M = Newtype1 (Array)

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
            (fmap
               (fun h t -> M.inj @@ Array.of_list @@ h :: (Array.to_list @@ M.prj t))
               u)
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
end
