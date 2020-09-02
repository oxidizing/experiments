module Config : sig

  type t =
    | Int
    | Bool
    | String

  type value =
    | Int of int
    | Bool of bool
    | String of string

  exception Configuration_error of string

  module type Keys = sig
    type t
    val get_name : t -> string
  end

  module type Provider = sig
    type key
    val get : key -> value option
  end

  module type S = sig
    type key
    val get : key -> value
  end

  module ChainedProvider (K: Keys) (P1: Provider with type key = K.t) (P2: Provider with type key = K.t) : Provider with type key = K.t
  module Make (K: Keys) (P: Provider with type key = K.t): S with type key = K.t
end = struct
  type t =
    | Int
    | Bool
    | String

  type value =
    | Int of int
    | Bool of bool
    | String of string

  exception Configuration_error of string

  module type Keys = sig
    type t
    val get_name : t -> string
  end

  module type Provider = sig
    type key
    val get : key -> value option
  end

  module type S = sig
    type key
    val get : key -> value
  end

  module ChainedProvider (K: Keys) (P1: Provider with type key = K.t) (P2: Provider with type key = K.t) = struct
    type key = K.t
    let get k =
      match (P1.get k) with
      | Some v -> Some v
      | None -> (P2.get k)
  end

  module Make (K: Keys) (P: Provider with type key = K.t) = struct
    type key = K.t
    let get k = match P.get k with
      | Some v -> v
      | None -> raise (Configuration_error (Fmt.strf "%s not set" (K.get_name k)))
  end
end

module Smtp = struct
  module Config = struct
    type key = [ `SMTP_USERNAME | `SMTP_PASSWORD | `SMTP_HOSTNAME | `SMTP_PORT | `SMTP_SENDER | `SMTP_STARTTLS ]

    let get_key_name : key -> string = function
      | `SMTP_USERNAME -> "SMTP_USERNAME"
      | `SMTP_PASSWORD -> "SMTP_PASSWORD"
      | `SMTP_HOSTNAME -> "SMTP_HOSTNAME"
      | `SMTP_PORT -> "SMTP_PORT"
      | `SMTP_SENDER -> "SMTP_SENDER"
      | `SMTP_STARTTLS -> "SMTP_STARTTLS"
  end
end

module WebServer = struct
  module Config = struct
    type key = [ `BIND_ADDRESS | `BIND_PORT ]

    let get_key_name : key -> string = function
      | `BIND_ADDRESS -> "BIND_ADDRESS"
      | `BIND_PORT -> "BIND_PORT"

  end
end

module FakeEnvConfigProvider : sig
  module Make (K: Config.Keys) : Config.Provider with type key = K.t
end = struct
  module Make (K: Config.Keys) = struct
    type key = K.t
    let get k =
      let key_name = K.get_name k in
      match key_name with
      | "SMTP_PORT" -> Some (Config.Int 2525)
      | "SMTP_STARTTLS" -> Some (Config.Bool true)
      | "SMTP_USERNAME" -> Some (Config.String "user")
      | "SMTP_PASSWORD" -> Some (Config.String "passwd")
      | _ -> None
  end
end

module EmptyConfigProvider : sig
  module Make (K: Config.Keys) : Config.Provider with type key = K.t
end = struct
  module Make (K: Config.Keys) = struct
    type key = K.t
    let get _ = None
  end
end

module AppKeys : Config.Keys with type t = [ Smtp.Config.key | WebServer.Config.key ] = struct
  type t = [ Smtp.Config.key | WebServer.Config.key ]

  let get_name : t -> string = fun key ->
      match key with
      | #Smtp.Config.key as k -> Smtp.Config.get_key_name k
      | #WebServer.Config.key as k -> WebServer.Config.get_key_name k
end

module EnvConfigProvider : Config.Provider with type key = AppKeys.t = FakeEnvConfigProvider.Make (AppKeys)
module DummyConfigProvider : Config.Provider with type key = AppKeys.t = EmptyConfigProvider.Make (AppKeys)
module ConfigProvider : Config.Provider with type key = AppKeys.t = Config.ChainedProvider (AppKeys) (EnvConfigProvider) (DummyConfigProvider)
module AppConfig : Config.S with type key = AppKeys.t = Config.Make (AppKeys) (ConfigProvider)

module Test = struct
  let pp : Format.formatter -> Config.value -> unit = fun fmt value -> match value with
      | Int i -> Fmt.fmt "%s" fmt (string_of_int i)
      | String s -> Fmt.fmt "%s" fmt s
      | Bool b -> Fmt.fmt "%s" fmt (string_of_bool b)
  let equal : Config.value -> Config.value -> bool = fun a b -> match a, b with
      | Int a, Int b -> a == b
      | String a, String b -> a == b
      | Bool a, Bool b -> a == b
      | _, _ -> false
  let value = Alcotest.testable pp equal
end

let test_int_value _ () =
  Alcotest.check Test.value "value matches" (Config.Int 2525) (AppConfig.get `SMTP_PORT);
  Lwt.return ()

let test_bool_value _ () =
  Alcotest.check Test.value "value matches" (Config.Bool true) (AppConfig.get `SMTP_STARTTLS);
  Lwt.return ()

let test_string_value _ () =
  Alcotest.check Test.value "value matches" (Config.String "user") (AppConfig.get `SMTP_USERNAME);
  Lwt.return ()

let test_missing_value _ () =
  Alcotest.check_raises "Foo" (Config.Configuration_error "SMTP_HOSTNAME not set") (fun () -> let _ = AppConfig.get `SMTP_HOSTNAME in ());
  Lwt.return ()

let () =
  Lwt_main.run
    (Alcotest_lwt.run "Config tests"
       [
         ( "Something", [
             Alcotest_lwt.test_case "test getting int value" `Quick test_int_value;
             Alcotest_lwt.test_case "test getting bool value" `Quick test_bool_value;
             Alcotest_lwt.test_case "test getting string value" `Quick test_string_value;
             Alcotest_lwt.test_case "test getting missing value" `Quick test_missing_value;
           ]
         );
       ])
