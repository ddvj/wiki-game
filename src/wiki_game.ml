open! Core

(* [get_linked_articles] should return a list of wikipedia article lengths contained in
   the input.

   Note that [get_linked_articles] should ONLY return things that look like wikipedia
   articles. In particular, we should discard links that are:
   - Wikipedia pages under special namespaces that are not articles (see
     https://en.wikipedia.org/wiki/Wikipedia:Namespaces)
   - other Wikipedia internal URLs that are not articles
   - resources that are external to Wikipedia
   - page headers

   One nice think about Wikipedia is that stringent content moderation results in
   uniformity in article format. We can expect that all Wikipedia article links parsed
   from a Wikipedia page will have the form "/wiki/<TITLE>". *)
let namespace_helper link =
  match Wikipedia_namespace.namespace link with None -> true | _ -> false
;;

let get_linked_articles contents : string list =
  let open Soup in
  let all_links =
    parse contents
    $$ "a"
    |> to_list
    |> List.map ~f:(fun a ->
      match attribute "href" a with
      | Some x -> x
      | _ -> failwith "unexpected none")
  in
  let pre_filtered_list = List.filter all_links ~f:namespace_helper in
  List.remove_consecutive_duplicates
    (List.filter pre_filtered_list ~f:(String.is_prefix ~prefix:"/wiki/"))
    ~equal:String.equal
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

(* [visualize] should explore all linked articles up to a distance of [max_depth] away
   from the given [origin] article, and output the result as a DOT file. It should use the
   [how_to_fetch] argument along with [File_fetcher] to fetch the articles so that the
   implementation can be tested locally on the small dataset in the ../resources/wiki
   directory. *)
(*module Site = struct
  type t =
    { name : string
    ; url : string
    }
  [@@deriving compare, sexp, hash, equal]

  let to_string t = t.name

  let of_string s =
    { name =
        (let open Soup in
         parse (File_fetcher.fetch_exn (Local (File_path.of_string "../resources/")) ~resource:s)
         $ "title"
         |> R.leaf_text)
    ; url = s
    }
  ;;

  let get_url t = t.url
end*)

module Link = struct
  type t = string * string [@@deriving sexp, hash, compare]
end

module G = Graph.Imperative.Graph.Concrete (String)

module Dot = Graph.Graphviz.Dot (struct
    include G

    let edge_attributes _ = [ `Dir `Back ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

module LinkSet = Hash_set.Make (Link)
module SiteSet = Hash_set.Make (String)

let get_title ~how_to_fetch ~url =
  let open Soup in
  parse (File_fetcher.fetch_exn how_to_fetch ~resource:url)
  $ "title"
  |> R.leaf_text
  |> String.tr ~target:' ' ~replacement:'_'
  |> String.filter ~f:(fun char ->
    not (phys_equal char '(' || phys_equal char ')' || phys_equal char '-'))
;;

let rec get_edges ~depth ~origin ~how_to_fetch ~visited : LinkSet.t =
  if not (phys_equal depth 0)
  then (
    Hash_set.add visited origin;
    let final_set = LinkSet.create () in
    let adjacent_sites =
      get_linked_articles
        (File_fetcher.fetch_exn how_to_fetch ~resource:origin)
    in
    List.iter adjacent_sites ~f:(fun site ->
      Hash_set.add final_set (origin, site));
    List.fold adjacent_sites ~init:final_set ~f:(fun acc site ->
      Hash_set.union
        acc
        (get_edges ~depth:(depth - 1) ~origin:site ~how_to_fetch ~visited)))
  else LinkSet.create ()
;;

let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  let links =
    get_edges
      ~depth:max_depth
      ~origin
      ~how_to_fetch
      ~visited:(SiteSet.create ())
  in
  let graph = G.create () in
  Hash_set.iter links ~f:(fun (site1, site2) ->
    G.add_edge
      graph
      (get_title ~how_to_fetch ~url:site1)
      (get_title ~how_to_fetch ~url:site2));
  Dot.output_graph
    (Out_channel.create (File_path.to_string output_file))
    graph
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* [find_path] should attempt to find a path between the origin article and the
   destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with [File_fetcher] to fetch
   the articles so that the implementation can be tested locally on the small dataset in
   the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the graph. *)
let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
  ignore (max_depth : int);
  ignore (origin : string);
  ignore (destination : string);
  ignore (how_to_fetch : File_fetcher.How_to_fetch.t);
  failwith "TODO"
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Play wiki game by finding a link between the origin and destination \
       pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination =
        flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
