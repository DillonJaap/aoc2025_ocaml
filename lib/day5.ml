open Core
open Angstrom

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let parse_ingredients =
  let parse_int = take_while1 is_digit >>| int_of_string in
  let parse_range =
    let* start_range = parse_int <* char '-' in
    let* end_range = parse_int in
    return (start_range, end_range)
  in
  let* ranges = sep_by (char '\n') parse_range <* end_of_line <* end_of_line in
  let* ingredient_ids =
    sep_by (char '\n') parse_int <* end_of_line <* end_of_input
  in
  return (ranges, ingredient_ids)
;;

module Part1 = struct
  let is_fresh ranges id =
    List.exists ranges ~f:(fun a ->
      let s, e = a in
      s <= id && id <= e)
  ;;

  let run () =
    let ranges, ingredient_ids = Util.parse_file parse_ingredients "day5.txt" in
    List.sum
      (module Int)
      ingredient_ids
      ~f:(fun id -> if is_fresh ranges id then 1 else 0)
  ;;
end

module Part2 = struct
  (* intervals are inclusive *)
  module Interval = struct
    type t = int * int

    (* combine two intervals into a single interval if possible *)
    let combine a b =
      let a_start, a_end = a in
      let b_start, b_end = b in
      if a_end < b_start || b_end < a_start
      then (
        print_string "here";
        None)
      else Some (Int.min a_start b_start, Int.max a_end b_end)
    ;;

    let difference a b =
      let a_start, a_end = a in
      let b_start, b_end = b in
      if a_end < b_start || b_start < b_end
      then 0
      else (
        let begin_range = if a_start < b_start then b_start - a_start else 0 in
        let end_range = if b_end < a_end then a_end - b_end else 0 in
        begin_range + end_range)
    ;;

    (* let add interval_list interval = *)
    (*   let rec loop old_list new_list (interval : t) = *)
    (*     match List.hd old_list with *)
    (*     | None -> List.rev (interval :: new_list) *)
    (*     | Some hd -> *)
    (*       (match combine interval hd with *)
    (*        | None -> loop (List.tl_exn old_list) (hd :: new_list) interval *)
    (*        | Some combined -> loop (List.tl_exn old_list) new_list combined) *)
    (*   in *)
    (*   loop interval_list [] interval *)
  end

  let run () =
    let ranges, _ = Util.parse_file parse_ingredients "day5.txt" in
    print_s ([%sexp_of: (int * int) list] ranges);
    (* let ranges = Interval.add ranges (3, 5) in *)
    (* print_s ([%sexp_of: (int * int) list] ranges); *)
    (* let new_range = Interval.combine (3, 3) (4, 11) in *)
    (* print_s ([%sexp_of: (int * int) option] new_range); *)
    0
  ;;
  (* List.sum *)
  (*   (module Int) *)
  (*   ranges *)
  (*   ~f:(fun range -> *)
  (*     let s, e = range in *)
  (*     e - s + 1) *)
end
