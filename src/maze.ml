open! Core

type state =
  | Start
  | End
  | Wall
  | Open
  | Path

module Node = struct
  type t =
    { mutable state : state
    ; x : int
    ; y : int
    }

  let to_string t =
    match t.state with
    | Start -> "S"
    | End -> "E"
    | Wall -> "#"
    | Path -> "$"
    | Open -> "."
  ;;

  let get_state t = t.state
  let x t = t.x
  let y t = t.y
end

module Maze = struct
  type t =
    { contents : Node.t list list
    ; width : int
    ; height : int
    }

  let get_contents t = t.contents
end

let print_row (row : Node.t list) : unit =
  List.iter row ~f:(fun node -> Node.to_string node |> print_string);
  print_endline ""
;;

let print_maze (maze : Maze.t) : unit =
  List.rev (Maze.get_contents maze) |> List.iter ~f:print_row
;;

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () ->
        ignore (input_file : File_path.t);
        failwith "TODO"]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
