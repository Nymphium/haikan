open Higher

module Semigroup = struct
  module type S = sig
    type t

    val mconcat : t -> t -> t
  end

  module type Operator = sig
    type t

    val ( <> ) : t -> t -> t
  end

  module type S' = sig
    type t

    include S with type t := t
    include Operator with type t := t
  end

  module Wrap (M : S) : S' with type t = M.t = struct
    include M

    let[@inline] ( <> ) l r = M.mconcat l r
  end
end

module Monoid = struct
  module type S = sig
    include Semigroup.S
    module Semigroup : Semigroup.S

    val mempty : t
  end
end

module Foldable = struct
  module type S = sig
    include Monoid.S
    module Monoid : Monoid.S

    type f

    val fold_map : ('a -> Monoid.t) -> ('a, f) app -> Monoid.t
    val foldr : ('a -> 'b -> 'b) -> 'b -> ('a, f) app -> 'b
  end
end

module Traverse = struct
  module type S = sig
    include Foldable.S
    module Foldable : Foldable.S
    include Control.Functor.S with type f := Foldable.f
    module Functor : Control.Functor.S with type f = Foldable.f

    val traverse
      :  (module Control.Applicative.S with type f = 'g)
      -> ('a -> ('b, 'g) app)
      -> ('a, f) app
      -> (('b, f) app, 'g) app

    val sequence_a
      :  (module Control.Applicative.S with type f = 'g)
      -> (('a, 'g) app, f) app
      -> (('a, f) app, 'g) app
  end
end
