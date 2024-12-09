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

let part2 _ = failwith "TODO"
