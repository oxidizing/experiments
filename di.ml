(*
 * An API for type-safe and composable dependencies.
 *)
module type S = sig
  module Context : sig
    type value

    type 'a t

    val value : 'a -> 'a t -> value
  end

  type (+'a, 'r) t

  type void

  val return : 'a -> ('a, 'r) t

  val map : ('a -> 'b) -> ('a, 'r) t -> ('b, 'r) t

  val run : ('a, void) t -> 'a

  val provide : ('r -> Context.value) -> ('a, 'r) t -> ('a, 's) t

  val fetch : tag:('a Context.t -> 'r) -> ('a, 'r) t

  val ( let* ) : ('a, 'r) t -> ('a -> ('b, 'r) t) -> ('b, 'r) t
end

(*
 *  Implementing the signature above.
 *)
module Task : S = struct
  module type T = sig
    type t
  end

  module Context = struct
    type value = exn

    type 'a t = 'a -> value

    let make f = f

    let value k e = e k

    module MakeBox (T : T) = struct
      exception Box of T.t

      let box (x : T.t) = Box x

      let unbox (v : value) =
        match v with Box b -> b | _ -> failwith "Invalid"
    end

    let embed (type a) () =
      let module B = MakeBox (struct
        type t = a
      end) in
      (B.box, B.unbox)
  end

  type ('a, 'r) t = 'r Context.t -> 'a

  type void

  let return x _ = x

  let join m e = (m e) e

  let map f m e = f @@ m e

  let bind m f = join @@ map f m

  let run t = t @@ Context.make (fun _ -> failwith "Unreachable")

  let fetch ~tag =
    let box, unbox = Context.embed () in
    let key = tag @@ Context.make box in
    fun ctx -> unbox @@ Context.value key ctx

  let provide f t _ = t @@ Context.make f

  let ( let* ) m f = bind m f
end

(*
 * Use the API for factor out simple configuration options.
 *)
module Example1 = struct
  open Task

  let save_to_database ~connection _ =
    print_endline connection;
    return ()

  let get_user_id () : (int, [> `User_id of int Context.t ]) t =
    fetch ~tag:(fun ctx -> `User_id ctx)

  type log_mode = Local | Remote

  let get_log_mode () : (log_mode, [> `Log_mode of log_mode Context.t ]) t =
    fetch ~tag:(fun ctx -> `Log_mode ctx)

  let get_connection_string () :
      (string, [> `Connection_string of string Context.t ]) t =
    fetch ~tag:(fun ctx -> `Connection_string ctx)

  let log s =
    let* mode = get_log_mode () in
    let* user_id = get_user_id () in
    return
      ( match mode with
      | Local -> Printf.printf "[User %d] %s" user_id s
      | Remote -> failwith "Not implemented" )

  let store_item item =
    let* connection = get_connection_string () in
    let* () = save_to_database ~connection item in
    log ("Saved item " ^ item)

  let program =
    let* () = store_item "My-item" in
    return ()

  let _ =
    program
    |> provide (function
         | `Connection_string ctx -> Context.value "abc123" ctx
         | `User_id ctx -> Context.value 123 ctx
         | `Log_mode ctx -> Context.value Local ctx)
    |> run
end

(*
 * Use the API to factor out module dependencies.
 *)
module Example2 = struct
  open Task

  module type Logging = sig
    val log : string -> unit
  end

  module type Database = sig
    val query : string -> unit
  end

  let log s =
    let* lm = fetch ~tag:(fun ctx -> `Logging ctx) in
    let module L = (val lm : Logging) in
    return @@ L.log s

  let query s =
    let* db = fetch ~tag:(fun ctx -> `Database ctx) in
    let module Db = (val db : Database) in
    return @@ Db.query s

  let service app =
    let* () = log "Register service" in
    app

  let app () =
    let* () = log "Starting app" in
    query "..."

  let my_service = service @@ app ()

  (* A production instance *)
  module Logger = struct
    let log = print_endline
  end

  module Database = struct
    let query _ = ()
  end

  let prod_program : (unit, void) t =
    my_service
    |> provide (function
         | `Logging ctx -> Context.value (module Logger : Logging) ctx
         | `Database ctx -> Context.value (module Database : Database) ctx)

  (* A test instance *)
  module MockLogger = struct
    let log = print_endline
  end

  module MockDatabase = struct
    let query _ = ()
  end

  let test_program : (unit, void) t =
    my_service
    |> provide (function
         | `Logging ctx -> Context.value (module MockLogger : Logging) ctx
         | `Database ctx -> Context.value (module MockDatabase : Database) ctx)
end
