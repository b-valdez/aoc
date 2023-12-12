open! Core

(* if I wouldn't insist on both identifier reflecting the name of the file
   and the command also working on windows, this could be done only using
   dune's DSL, but this way I can also escape the string *)
let () =
  let target = (Sys.get_argv ()).(1) in
  let identifier =
    Core.Filename.basename target |> String.take_while ~f:(Fn.non @@ [%equal: char] '.')
  in
  let blob = In_channel.read_all target in
  print_string {%string|let %{identifier} = "%{String.escaped blob}"|}
;;
