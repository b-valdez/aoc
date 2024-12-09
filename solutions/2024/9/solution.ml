open! Aoc_std

let parser =
  let open Angstrom in
  digit
;;

let add_to_checksum checksum block_id fs_pos block_len =
  Iter.(fs_pos -- (fs_pos + block_len - 1))
  |> Iter.fold ~init:checksum ~f:(fun checksum pos -> checksum + (pos * block_id))
;;

(* assuming the input ends with a file and not a free space *)
let part1 seq =
  let blocks = Iter.fold ~init:Fdeque.empty ~f:Fdeque.enqueue_back seq in
  let length = Fdeque.length blocks in
  assert (length mod 2 = 1);
  (* front_id is the id of the next file that will be read from the front, back_id is the id of the files in files_in_back *)
  let rec aux checksum blocks free_in_front files_in_back front_id back_id fs_pos =
    match () with
    | () when free_in_front = 0 ->
      let front, blocks = Fdeque.dequeue_front_exn blocks in
      let checksum = add_to_checksum checksum front_id fs_pos front in
      let fs_pos = fs_pos + front in
      let front_id = front_id + 1 in
      if front_id >= back_id
      then add_to_checksum checksum back_id fs_pos files_in_back
      else (
        let free_in_front, blocks = Fdeque.dequeue_front_exn blocks in
        let back_files_moved = min free_in_front files_in_back in
        let checksum = add_to_checksum checksum back_id fs_pos back_files_moved in
        (aux [@tailcall])
          checksum
          blocks
          (free_in_front - back_files_moved)
          (files_in_back - back_files_moved)
          front_id
          back_id
          (fs_pos + back_files_moved))
    | () when files_in_back = 0 ->
      let files_in_back, blocks = Fdeque.dequeue_back_exn blocks in
      let back_id = back_id - 1 in
      if front_id >= back_id
      then add_to_checksum checksum back_id fs_pos files_in_back
      else (
        let back_files_moved = min free_in_front files_in_back in
        let checksum = add_to_checksum checksum back_id fs_pos back_files_moved in
        let _, blocks = Fdeque.dequeue_back_exn blocks in
        (aux [@tailcall])
          checksum
          blocks
          (free_in_front - back_files_moved)
          (files_in_back - back_files_moved)
          front_id
          back_id
          (fs_pos + back_files_moved))
    | () -> assert false
  in
  aux 0 blocks 0 0 0 ((length + 1) / 2) 0
;;

let part2 seq =
  let spaces = Doubly_linked.create () in
  let files, _, id =
    Iter.foldi seq ~init:([], 0, -1) ~f:(fun (files, pos, id) i block ->
      if i mod 2 = 0
      then (block, pos) :: files, pos + block, id + 1
      else if block = 0
      then files, pos, id
      else (
        let (_ : _ Doubly_linked.Elt.t) = Doubly_linked.insert_last spaces (block, pos) in
        files, pos + block, id))
  in
  let delete_last_if_it_ends_at end_pos =
    let%bind.Option size, start_pos = Doubly_linked.last spaces in
    if start_pos + size = end_pos then Doubly_linked.remove_last spaces else None
  in
  let rec aux checksum too_big_to_move id = function
    | [] -> checksum
    | files when too_big_to_move = 1 ->
      (* no moves are possible, just compute the remaining checksum *)
      List.fold files ~init:(checksum, id) ~f:(fun (checksum, id) (size, pos) ->
        add_to_checksum checksum id pos size, id - 1)
      |> fst
    | (size, pos) :: files when size >= too_big_to_move ->
      let _ : _ option = delete_last_if_it_ends_at pos in
      let checksum = add_to_checksum checksum id pos size in
      (aux [@tailcall]) checksum too_big_to_move (id - 1) files
    | (size, pos) :: files ->
      (match Doubly_linked.find_elt spaces ~f:(fun (space, _) -> size <= space) with
       | None ->
         let _ : _ option = delete_last_if_it_ends_at pos in
         let checksum = add_to_checksum checksum id pos size in
         (aux [@tailcall]) checksum size (id - 1) files
       | Some space_container ->
         let deleted_pos =
           delete_last_if_it_ends_at pos |> Option.value_map ~default:0 ~f:snd
         in
         let space, space_pos = Doubly_linked.Elt.value space_container in
         let checksum = add_to_checksum checksum id space_pos size in
         if deleted_pos <> space_pos
         then
           if space = size
           then Doubly_linked.remove spaces space_container
           else Doubly_linked.Elt.set space_container (space - size, space_pos + size);
         (aux [@tailcall]) checksum too_big_to_move (id - 1) files)
  in
  aux 0 Int.max_value id files
;;
