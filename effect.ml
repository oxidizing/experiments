type ('r, 'a, 'e) xio = 'r -> ('a, 'e) result Lwt.t

type ('r, 'a) rio = 'r -> 'a Lwt.t

let bind : ('r, 'a, 'e) xio -> ('a -> ('r, 'b, 'e) xio) -> ('r, 'b, 'e) xio =
 fun e f r -> Lwt_result.bind (e r) (fun a -> f a r)

let map : ('a -> 'b) -> ('r, 'a, 'e) xio -> ('r, 'b, 'e) xio =
 fun e f r -> Lwt_result.map f (e r)

let map_env : ('s -> 'r) -> ('r, 'a, 'e) xio -> ('s, 'a, 'e) xio =
 fun f e r -> e (f r)

let map_err : ('r, 'a, 'e1) xio -> ('e1 -> 'e2) -> ('r, 'b, 'e2) xio =
 fun e f r -> Lwt_result.map_err f (e r)

let zip :
    ('r1, 'a1, 'e1) xio ->
    ('r2, 'a2, 'e1) xio ->
    ('r1 * 'r2, 'a1 * 'a2, 'e1) xio =
 fun e1 e2 (r1, r2) ->
  Lwt_result.bind (e1 r1) (fun a1 ->
      Lwt_result.map (fun a2 -> (a1, a2)) (e2 r2))

let return : 'a -> ('r, 'a, 'e) xio = fun a _ -> Lwt_result.return a

(* TODO evaluate Lazy.t for this *)

let of_result : ('a, 'e) Result.t -> (unit, 'a, 'e) xio =
 fun result () -> Lwt.return result

let of_lwt : (unit -> 'a Lwt.t) -> (unit, 'a, 'e) xio =
 fun lwt_f () -> Lwt.map (fun a -> Ok a) (lwt_f ())

let of_lwt_result : (unit -> ('a, 'e) Result.t Lwt.t) -> (unit, 'a, 'e) xio =
 fun a -> a

let of_rio : ('r, 'a) rio -> ('r, 'a, 'e) xio =
 fun rio r -> Lwt.map Result.ok (rio r)

let run : ('r, 'a, 'e) xio -> 'r -> unit =
 fun xio a -> Lwt_main.run (xio a |> Lwt.map Result.get_ok)

module Syntax = struct
  let ( let+ ) e f = map f e

  let ( and+ ) = zip

  let ( let* ) = bind

  let ( and* ) = zip
end
