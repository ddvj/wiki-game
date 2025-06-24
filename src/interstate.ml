open! Core
module City = String

module Network = struct
  module Connection = struct
    module T = struct
      type t = City.t * City.t [@@deriving compare, sexp]
    end

    include Comparable.Make (T)
  end

  type t = Connection.Set.t [@@deriving sexp_of]

  let of_file input_file =
    let connections =
      In_channel.read_lines (File_path.to_string input_file)
      (* removed periods from txt file because it was messing up dot file syntax *)
      |> List.map
           ~f:(String.filter ~f:(fun char -> not (Char.equal char '.')))
      (* replaced spaces with dashes for dot file syntax*)
      |> List.map
           ~f:
             (String.map ~f:(fun char ->
                match char with ' ' -> '-' | a -> a))
      |> List.map ~f:(fun s -> String.split ~on:',' s)
      |> List.map ~f:(fun lst -> List.tl lst |> Option.value_exn)
      |> List.concat_map ~f:(fun cities ->
        List.cartesian_product cities cities)
      |> List.filter ~f:(fun tuple ->
        match tuple with a, b -> not (String.equal a b))
    in
    Connection.Set.of_list connections
  ;;
end

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        let network = Network.of_file input_file in
        printf !"%{sexp: Network.t}\n" network]
;;

module G = Graph.Imperative.Graph.Concrete (City)

module Dot = Graph.Graphviz.Dot (struct
    include G

    let edge_attributes _ = [ `Dir `None ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing all interstates and the cities they go \
             through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        let network = Network.of_file input_file in
        let graph = G.create () in
        Set.iter network ~f:(fun (person1, person2) ->
          G.add_edge graph person1 person2);
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
