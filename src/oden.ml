open Lwt
open Cohttp
open Cohttp_lwt_unix
open Core.Std
open Re2.Std

let validate_header header =
  match Header.get header "content-type" with
  | Some s -> String.equal s "application/json"
  | None -> false

let pattern = Re2.create_exn "([0-9A-Za-z]+)_(\\d{8})\\.([0-9A-Za-z_]+)"

let parse_app_header header header_name =
  match Header.get header header_name with
  | None -> Or_error.errorf "%s rquired" header_name
  | Some s ->
      (match Re2.find_submatches pattern s with
      | Ok m ->
          let open Option.Monad_infix in
          (match m.(1) >>= fun m1 -> m.(2) >>= fun m2 -> m.(3) >>= fun m3 -> Some (m1, m2, m3) with
          | Some t -> Ok t
          | None -> Or_error.errorf "invalid format %s" s)
      | Error _ as e -> e)


let bad_request msg =
  Server.respond_error ~headers:(Header.init ()) ~status:`Bad_request ~body:(Printf.sprintf "Bad request: %s" msg) ()

let server header_name =
  let callback _conn req body =
    match req |> Request.meth with
    | `POST ->
        if validate_header (Request.headers req) then
          (match parse_app_header (Request.headers req) header_name with
          | Error e -> bad_request (Error.to_string_hum e)
          | Ok (service, version, operation) ->
          body |> Cohttp_lwt_body.to_string >|= (fun body ->
            (Printf.sprintf "%s\n%s\n%s\nBody: %s" service version operation body))
          >>= (fun body -> Server.respond_string ~status:`OK ~body ()))
        else
          bad_request "no header"
    | _ -> bad_request "invalid method"
    
  in
  Server.create ~mode:(`TCP (`Port 8999)) (Server.make ~callback ())

