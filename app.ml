open Effect.Syntax

type database

type user

type email_address

type email_transport

type logger

module Database : sig
  val connect : unit -> database
end = struct
  let connect = Obj.magic "tada"
end

module UserService : sig
  val email_address : user -> email_address

  val find_user : id:string -> (database, user) Effect.rio
end = struct
  let email_address = Obj.magic "tada"

  let find_user = Obj.magic "tada"
end

module EmailService : sig
  val send_email : email_address -> string -> (email_transport, unit) Effect.rio

  val connect : unit -> email_transport
end = struct
  let send_email = Obj.magic "tada"

  let connect = Obj.magic "tada"
end

module LogService : sig
  val log : string -> (logger, unit) Effect.rio

  val setup : unit -> logger
end = struct
  let log = Obj.magic "tada"

  let setup = Obj.magic "tada"
end

(* Pure function because of lazy effect type*)
let email_address_of_user user_id =
  let user = UserService.find_user ~id:user_id |> Effect.of_rio
  and logged =
    LogService.log ("Fetched user with id " ^ user_id) |> Effect.of_rio
  in
  let* user, () = Effect.zip user logged in
  UserService.email_address user |> Effect.return

(* Pure function because of lazy effect type *)
let send_email_to_user user_id message =
  let* email_address =
    email_address_of_user user_id
    |> Effect.map_env (fun (database, logger, _) -> (database, logger))
  in
  EmailService.send_email email_address message
  |> Effect.of_rio
  |> Effect.map_env (fun (_, _, email_transport) -> email_transport)

let program = send_email_to_user "1" "Hello there"

(* Dependency setup *)
let dependencies =
  let database = Database.connect () in
  let logger = LogService.setup () in
  let email_transport = EmailService.connect () in
  (database, logger, email_transport)

(* Only here I/O and dependency injection is happening *)
let () = Effect.run program dependencies
