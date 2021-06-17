open Higher
open Haikan
open Haikan.Ext

module Ops = struct
  type 'r t =
    | Double of int * (int -> 'r)
    | Write of string * (unit -> 'r)

  module M = Newtype1 (struct
    type nonrec 'a t = 'a t
  end)

  let double v k = Double (v, k) |> M.inj
  let write s k = Write (s, k) |> M.inj

  module Functor : Control.Functor.S with type f = M.t = struct
    type f = M.t

    let fmap f m =
      M.inj
      @@
      match M.prj m with
      | Double (i, k) -> Double (i, fun v -> k v |> f)
      | Write (s, k) -> Write (s, fun v -> k v |> f)
    ;;
  end
end

let interpret v =
  v
  |> Free.interpret'
     @@ fun interpret ops ->
     match Ops.M.prj ops with
     | Double (v, fr) -> interpret @@ fr (v * v)
     | Write (s, fr) ->
       print_endline s;
       interpret @@ fr ()
;;

let p =
  let open Free.Monad (Ops.Functor) in
  let* v = Free.perform Ops.double 3 in
  let* () = Free.perform Ops.write @@ string_of_int v in
  return ()
;;

let () = interpret p
