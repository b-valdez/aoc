open! Aoc_std

type op =
  | Add
  | Multiply
[@@deriving sexp]

let parser =
  let open Angstrom in
  let+ numbers =
    many1_till
      (take_till (Char.equal '\n') <* end_of_line <* commit)
      (peek_char_fail
       >>= function
       | '+' | '*' -> return ()
       | _ -> fail "there are still more number lines")
  and+ operations =
    many1
      (both
         (char '+' *> return Add <|> char '*' *> return Multiply <* commit)
         (lift2
            (fun spaces at_end_of_input ->
               String.length spaces + if at_end_of_input then 1 else 0)
            (take_while (Char.equal ' '))
            at_end_of_input))
    <* end_of_input
  in
  operations, numbers
;;

let part1 (operations, numberss) =
  let rec aux grand_sum offset = function
    | [] -> grand_sum
    | (op, len) :: operations ->
      let init, op =
        match op with
        | Add -> 0, ( + )
        | Multiply -> 1, ( * )
      in
      let result =
        List.fold numberss ~init ~f:(fun result numbers ->
          let operatee =
            String.slice numbers offset (offset + len) |> String.strip |> Int.of_string
          in
          op result operatee)
      in
      (aux [@tailcall]) (grand_sum + result) (offset + len + 1) operations
  in
  aux 0 0 operations
;;

let part2 (operations, numberss) =
  let rec aux grand_sum offset = function
    | [] -> grand_sum
    | (op, len) :: operations ->
      let init, op =
        match op with
        | Add -> 0, ( + )
        | Multiply -> 1, ( * )
      in
      let result =
        Iter.(offset -- (offset + len - 1))
        |> Iter.fold ~init ~f:(fun result offset ->
          let operatee =
            List.fold_until
              numberss
              ~init:0
              ~f:(fun acc numbers ->
                match numbers.[offset] with
                | ' ' when acc = 0 -> Continue 0
                | ' ' -> Stop acc
                | digit -> Continue ((10 * acc) + Char.get_digit_exn digit))
              ~finish:Fun.id
          in
          op result operatee)
      in
      (aux [@tailcall]) (grand_sum + result) (offset + len + 1) operations
  in
  aux 0 0 operations
;;
