open! Core
open Shexp_process

let args =
  let default_year =
    let%map.Shexp_process year = get_env "AOC_YEAR" in
    year
    |> Option.map ~f:Int.of_string
    |> Option.value_or_thunk ~default:(fun () ->
      Date.today ~zone:(Time_float.Zone.of_utc_offset ~hours:~-5)
      |> Fn.flip Date.add_months ~-11
      |> Date.year)
  in
  let default_day =
    let open Let_syntax in
    cwd_logical >>= readdir >>| Core.List.length >>| ( + ) 1
  in
  match%map_open.Command
    anon @@ maybe (t2 ("day or year" %: int) (maybe ("day" %: int)))
  with
  | None -> default_year, default_day
  | Some (day, None) -> default_year, return day
  | Some (year, Some day) -> return year, return day
;;

let main =
  let open Shexp_process.Let_syntax in
  let year, day =
    (Command.Param.parse args @@ Core.List.(of_array (Sys.get_argv ()) |> tl_exn))
    |> Or_error.ok_exn
  in
  let%bind year = year >>| string_of_int in
  let mkchdir name k =
    let%bind () = if%bind file_exists name then return () else mkdir name in
    chdir name k
  in
  let create_day day =
    let%bind day = day >>| string_of_int in
    let libname = {%string|aoc_%{year}_%{day}|} in
    mkdir day
    >> chdir
         day
         (stdout_to "sample.blob" (echo ~n:() "")
          >> stdout_to "input.blob" (echo ~n:() "")
          >> stdout_to "dune" (echo ~n:() @@ Create_day_templates.dune libname)
          >> stdout_to "solution.ml" (echo ~n:() @@ Create_day_templates.solution)
          >> stdout_to "test.ml" (echo ~n:() @@ Create_day_templates.test))
  in
  mkchdir "solutions" (mkchdir year (create_day day))
;;

let () = eval main
