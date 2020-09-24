
module Core : sig
  type id

  module type Context = sig
    type t
  end

  val id_of_int : int -> id
end = struct
  type id = int

  module type Context = sig
    type t
  end

  let id_of_int i = i
end

module Users : sig
  module type S = sig
    type ctx
    type t

    val get_by_id : ctx -> Core.id -> t option
    val get_by_username : ctx -> string -> t option
    val password_match : ctx -> t -> string -> bool
    val get_username : ctx -> t -> string
  end

  module Make (C: Core.Context) : S with type ctx = C.t
end = struct
  module type S = sig
    type ctx
    type t

    val get_by_id : ctx -> Core.id -> t option
    val get_by_username : ctx -> string -> t option
    val password_match : ctx -> t -> string -> bool
    val get_username : ctx -> t -> string
  end

  module Make (C: Core.Context) = struct
    type ctx = C.t
    type t = {
      id: Core.id;
      username: string;
      password: string;
    }

    let u1 = {
      id = Core.id_of_int 1;
      username = "john";
      password = "passwd";
    }

    let get_by_id _ id = if id == u1.id then Some(u1) else None
    let get_by_username _ username = if username == u1.username then Some(u1) else None
    let password_match _ user password = if user.id == u1.id && password == u1.password then true else false
    let get_username _ user = user.username
  end
end

module Authn : sig

  module type S = sig
    type ctx
    type user

    val authenticate : ctx -> username:string -> password:string -> user option
  end

  module Make (C: Core.Context) (U : Users.S with type ctx = C.t) : S with type ctx = C.t and type user = U.t
end = struct
  module type S = sig
    type ctx
    type user

    val authenticate : ctx -> username:string -> password:string -> user option
  end

  module Make (C: Core.Context) (U: Users.S with type ctx = C.t) = struct
    type ctx = C.t
    type user = U.t

    let authenticate ctx ~username ~password =
      let user_op = U.get_by_username ctx username in
      match user_op with
      | Some u -> if U.password_match ctx u password then Some(u) else None
      | None -> None
  end
end

module Authz : sig
  type op = READ | WRITE | LIST

  module type S = sig
    type ctx
    val authorize : ctx -> Core.id -> op -> Core.id -> bool
  end

  module Make (C: Core.Context) : S with type ctx = C.t
end = struct
  type op = READ | WRITE | LIST

  module type S = sig
    type ctx
    val authorize : ctx -> Core.id -> op -> Core.id -> bool
  end

  module Make (C: Core.Context) = struct
    type ctx = C.t
    let authorize _ _ _ _ = true
  end
end

module rec Ctx : sig
  include Core.Context

  module Authn : Authn.S with type ctx = t
  module Users : Users.S with type ctx = t
  module Authz : Authz.S with type ctx = t

  val empty : unit -> t
end = struct
  type t = { authenticated_user: UsersImpl.t option }

  module Users = UsersImpl
  module Authn = AuthnImpl
  module Authz = AuthzImpl

  let empty () = { authenticated_user = None }
end
and UsersImpl : (Users.S with type ctx = Ctx.t) = Users.Make (Ctx)
and AuthnImpl : (Authn.S with type ctx = Ctx.t and type user = UsersImpl.t) = Authn.Make (Ctx) (UsersImpl)
and AuthzImpl : (Authz.S with type ctx = Ctx.t) = Authz.Make (Ctx)

let test_authorize () =
  let ctx = Ctx.empty () in
  Alcotest.(check bool) " is " true (Ctx.Authz.authorize ctx (Core.id_of_int 1) Authz.READ (Core.id_of_int 2))

let test_get_user_by_id () =
  let ctx = Ctx.empty () in
  let user_op = Ctx.Users.get_by_id ctx (Core.id_of_int 1) in
  match user_op with
  | Some u -> Alcotest.(check string) " username matches " "john" (Ctx.Users.get_username ctx u)
  | None -> Alcotest.fail "expecting some user, got none"

let () =
  let open Alcotest in
  run "ServiceContext" [
    "Users", [
      test_case "get user by id" `Quick test_get_user_by_id;
    ];
    "Authz", [
      test_case "authorize" `Quick test_authorize;
    ];
  ]
