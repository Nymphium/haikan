open struct
  module String = Stdlib.String
end

open Higher
module M = Newtype0 (String)
include M

module Semigroup : Data.Semigroup.S' with type t = M.t = Data.Semigroup.Wrap (struct
  type t = M.t

  let mconcat l r = M.inj @@ M.prj l ^ M.prj r
end)

module Monoid : Data.Monoid.S with type t = M.t = struct
  module Semigroup = Semigroup
  include Semigroup

  let mempty = M.inj ""
end
