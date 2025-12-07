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
  (* return the interval in a that is not in b *)
  let difference a b =
    let a_start, a_end = a in
    let b_start, b_end = b in
    if a_end < b_start || b_end < a_start
    then [ a ]
    else (
      let beg_range =
        if a_start < b_start then Some (a_start, b_start - 1) else None
      in
      let end_range = if b_end < a_end then Some (b_end + 1, a_end) else None in
      match beg_range, end_range with
      | Some b, Some e -> [ b; e ]
      | Some b, None -> [ b ]
      | None, Some e -> [ e ]
      | None, None -> [])
  ;;

  let remove_overlap list =
    let rec diff_list list intervals =
      match list, intervals with
      (* we either have recursed through the whole list *)
      (* or the list of intervals has become empty, in which case we can exit early *)
      | [], _ | _, [] -> intervals
      | hd :: tl, _ ->
        let intervals =
          List.fold intervals ~init:[] ~f:(fun acc cur ->
            List.concat [ acc; difference cur hd ])
        in
        diff_list tl intervals
    in
    (* apply the difference to every interval in the list to the rest of the list *)
    let rec loop list new_list =
      match list with
      | hd :: tl ->
        let diff = diff_list tl [ hd ] in
        loop tl (List.concat [ new_list; diff ])
      | _ -> new_list
    in
    loop list []
  ;;

  let run () =
    let intervals, _ = Util.parse_file parse_ingredients "day5.txt" in
    intervals
    |> remove_overlap
    |> List.fold ~init:0 ~f:(fun acc cur ->
      let start, end_ = cur in
      acc + (end_ - start) + 1)
  ;;
end
