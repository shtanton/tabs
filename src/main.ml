open Notty
open Notty_lwt
open Notty.Infix
open Lwt.Infix

type tabState =
  | Show
  | Hide

type tab = {
  name: string;
  state: tabState;
  show: string;
  hide: string;
  close: string;
}

type maybeTab = {
  name: string option;
  show: string option;
  hide: string option;
  close: string option;
}

type state = tab list

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

let mock text = {
  name=text;
  show="";
  hide="";
  close="";
  state=Show;
}

let newTab ?(state=Show) text oldState =
  let json = match Yojson.Basic.from_string text with
  | exception Yojson.Json_error e -> `String e
  | json -> json in
  let newTab = match json with
  | `Assoc vals -> (
    let maybeTab = List.fold_left (fun acc (key, v) ->
      match key, v with
      | "name", `String v -> {acc with name=Some v}
      | "show", `String v -> {acc with show=Some v}
      | "hide", `String v -> {acc with hide=Some v}
      | "close", `String v -> {acc with close=Some v}
      | _ -> acc
    ) {name=None; show=None; hide=None; close=None} vals in
    match maybeTab with
    | {
      name=Some name;
      show=Some show;
      hide=Some hide;
      close=Some close;
    } -> Some {
      name;
      show;
      hide;
      close;
      state;
    }
    | _ -> Some (mock "Missing")
  )
  | _ -> None in
  match newTab with
  | None -> oldState
  | Some tab -> tab :: oldState


let letterOfVal v = v + 65 |> Char.chr |> String.make 1

let imageOfTab letterVal {state; name; _} =
  let colour = match state with
  | Show -> A.white
  | Hide -> A.yellow in
  let letter = letterOfVal letterVal in
  let stringImage = I.string A.(fg colour) name in
  let keyImage = I.string A.(fg red) (letter ^ " ") in
  let hPadded = I.hcrop (-1) (-1) (keyImage <|> stringImage) in
  let vPadded = I.vcrop (-1) (-1) hPadded in
  vPadded

let rec getImage ?(letterNum=0) ?image:(image=I.empty) tabs =
  match tabs with
  | [] -> image
  | tab :: cons -> getImage ~letterNum:(letterNum + 1) ~image:(image <|> imageOfTab letterNum tab) cons

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
  Lwt_stream.to_list (Lwt_io.read_lines ic) >|= List.fold_left (^) ""

let getTerminalEvent () =
  Lwt_stream.next termEvents >|= (fun e -> TerminalEvent e)

let rec removeByIndex ?(index=0) ?(acc=[]) target state=
  match state with
  | [] -> acc
  | tab :: state ->
      if index=target then List.rev_append acc state else removeByIndex ~index:(index+1) ~acc:(tab :: acc) target state

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
          | ServerEvent text -> (Update (newTab text state), (getSocketEvent ()) :: events)
          | TerminalEvent (`Key (`ASCII ch, _)) -> (Update ((removeByIndex (Char.code ch - 97) state)), (getTerminalEvent ()) :: events)
          | TerminalEvent _ -> (Loop, (getTerminalEvent ()) :: events) in
          (action, List.rev_append (List.map Lwt.return es) events)
        ) in
      match action with
      | Quit -> Lwt.return ()
      | Loop -> loop ~state events
      | Update state -> loop ~state events
    ) in
  loop [getTerminalEvent (); getSocketEvent ()]

let () = Lwt_main.run (Server.createServer startServer)
