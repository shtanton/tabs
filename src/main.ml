open Notty
open Notty_lwt
open Notty.Infix
open Lwt.Infix

type state = string list

type action =
  | Quit
  | Update of state
  | Loop

type terminalEvent =
  [ `Key of Unescape.key
  | `Mouse of Unescape.mouse
  | `Paste of Unescape.paste
  | `Resize of int * int]

type event =
  | TerminalEvent of terminalEvent
  | ServerEvent of string

let imageOfString s =
  let stringImage = I.string A.(fg magenta) s in
  let hPadded = I.hcrop (-1) (-1) stringImage in
  let vPadded = I.vcrop (-1) (-1) hPadded in
  vPadded

let rec getImage ?image:(image=I.empty) tabs =
  match tabs with
  | [] -> image
  | tab :: cons -> getImage ~image:(image <|> imageOfString tab) cons

let t = Term.create ()
let termEvents = Term.events t

let render state =
  Term.image t (getImage state)

let sockaddr =
  let addr = "127.0.0.1" in
  let port = 12345 in
  Unix.ADDR_INET (Unix.inet_addr_of_string addr, port)

let handleConnection sock =
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input sock in
  Lwt_io.read_line ic >>= (fun s ->
    Lwt_unix.close sock >|= (fun () -> s)
  )

let getTerminalEvent () =
  Lwt_stream.next termEvents >|= (fun e -> TerminalEvent e)

let startServer sock =
  let getSocketEvent () = Lwt_unix.accept sock
    >>= (fun (s, _) -> handleConnection s) >|= (fun s -> ServerEvent s) in
  let rec loop ?(state=[]) events =
    if events=[] then Lwt.return_unit else
    render state >>= fun () ->
    Lwt.nchoose_split events >>= (fun (es, events) ->
      let (action, events) = match es with
        | [] -> (Loop, List.rev_append (List.map Lwt.return es) events)
        | e :: es -> (
          let (action, events) = match e with
          | TerminalEvent (`Key (`Escape, _)) -> (Quit, [])
          | ServerEvent text -> (Update (text :: state), (getSocketEvent ()) :: events)
          | TerminalEvent _ -> (Loop, (getTerminalEvent ()) :: events) in
          (action, List.rev_append (List.map Lwt.return es) events)
        ) in
      match action with
      | Quit -> Lwt.return ()
      | Loop -> loop ~state events
      | Update state -> loop ~state events
    ) in
  loop [getTerminalEvent (); getSocketEvent ()]

(*let () = Lwt_main.run (Lwt_stream.fold_s update termEvents [] >|= ignore)*)

let () = Lwt_main.run (Server.createServer startServer)
