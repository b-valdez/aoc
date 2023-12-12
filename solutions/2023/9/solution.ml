open! Aoc_std

let extrapolate list =
  let rec extrapolate sgn acc_hd acc_tl seq =
    let hd = List.hd_exn seq in
    let (all0, last), diffs =
      List.fold_map
        (List.tl_exn seq)
        ~init:(hd = 0, hd)
        ~f:(fun (all0, last) el -> (all0 && el = 0, el), el - last)
    in
    if all0
    then acc_hd, acc_tl
    else (extrapolate [@tailcall]) ~-sgn (acc_hd + (sgn * hd)) (last + acc_tl) diffs
  in
  (extrapolate [@tailcall]) 1 0 0 list
;;

let parser =
  let open Angstrom in
  lines (sep_by1 (char ' ' <* commit) int) >>| List.map ~f:extrapolate
;;

let part1 = List.sum (module Int) ~f:snd
let part2 = List.sum (module Int) ~f:fst

let%expect_test "sample" =
  let parsed = parse_string parser Sample.sample in
  printf "%d" @@ part1 parsed;
  [%expect {| 114 |}];
  printf "%d" @@ part2 parsed;
  [%expect {| 2 |}]
;;

let%expect_test "input" =
  let parsed = parse_string parser Input.input in
  printf "%d" @@ part1 parsed;
  [%expect {| 1584748274 |}];
  printf "%d" @@ part2 parsed;
  [%expect {| 1026 |}]
;;
