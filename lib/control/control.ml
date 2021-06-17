open Higher

module Functor = struct
  module type S = sig
    type f

    val fmap : ('a -> 'b) -> ('a, f) app -> ('b, f) app
  end

  module type Operator = sig
    type f

    val ( <$> ) : ('a -> 'b) -> ('a, f) app -> ('b, f) app
  end

  module type S' = sig
    type f

    include S with type f := f
    include Operator with type f := f
  end

  module Wrap (M : S) : S' with type f = M.f = struct
    include M

    let[@inline] ( <$> ) f m = M.fmap f m
  end
end

module Applicative = struct
  module type S = sig
    include Functor.S
    module Functor : Functor.S

    val app : ('a -> 'b, f) app -> ('a, f) app -> ('b, f) app
    val pure : 'a -> ('a, f) app
  end

  module type Operator = sig
    type f

    val ( <*> ) : ('a -> 'b, f) app -> ('a, f) app -> ('b, f) app
  end

  module type S' = sig
    type f

    include S with type f := f
    include Operator with type f := f
  end

  module Wrap (M : S) : S' with type f = M.f = struct
    include M

    let[@inline] ( <*> ) fab fa = M.app fab fa
  end
end

module Monad = struct
  module type S = sig
    include Applicative.S
    module Applicative : Applicative.S

    val bind : ('a, f) app -> ('a -> ('b, f) app) -> ('b, f) app
    val return : 'a -> ('a, f) app
  end

  module type Operator = sig
    type f

    val ( >>= ) : ('a, f) app -> ('a -> ('b, f) app) -> ('b, f) app
    val ( =<< ) : ('a -> ('b, f) app) -> ('a, f) app -> ('b, f) app
    val ( let* ) : ('a, f) app -> ('a -> ('b, f) app) -> ('b, f) app
  end

  module type S' = sig
    type f

    include S with type f := f
    include Operator with type f := f
  end

  module Wrap (M : S) : S' with type f = M.f = struct
    include M

    let[@inline] ( >>= ) m k = M.bind m k
    let[@inline] ( =<< ) k m = M.bind m k
    let ( let* ) m k = M.bind m k
  end
end
