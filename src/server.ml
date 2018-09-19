open Lwt.Infix

let handleConnection fn sock =
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input sock in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output sock in
  fn ic oc

let rec acceptConnections fn sock =
  Lwt_unix.accept sock >>= fun (s, _) ->
    let () = fn s in
    acceptConnections fn sock

let sockaddr = Lwt_unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 12345)
let domain = Unix.domain_of_sockaddr sockaddr

let createServer fn =
  let sock = Lwt_unix.socket domain Unix.SOCK_STREAM 0 in
  Lwt_unix.bind sock sockaddr >>= fun () ->
    Lwt_unix.listen sock 1;
    fn sock
