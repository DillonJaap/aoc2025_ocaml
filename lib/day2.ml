open Core
open Angstrom

(* Parsing *)
let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let parse_ranges =
  let parse_range =
    let* start_range = take_while1 is_digit <* char '-' in
    let* end_range = take_while1 is_digit in
    return (int_of_string start_range, int_of_string end_range)
  in
  sep_by (char ',') parse_range <* end_of_line <* end_of_input
;;

module Part1 = struct
  let has_twice_repeated_digits number =
    let number = string_of_int number in
    match String.length number with
    | len when len % 2 = 1 -> false
    | len ->
      String.( = )
        (String.slice number 0 (len / 2)) (* first half *)
        (String.slice number (len / 2) len (* second half *))
  ;;

  let sum_in_range range_start range_end =
    let rec loop current sum =
      match current <= range_end, has_twice_repeated_digits current with
      | true, true -> loop (current + 1) sum + current
      | true, false -> loop (current + 1) sum
      | false, _ -> sum
    in
    loop range_start 0
  ;;

  let sum_ranges ranges =
    List.fold ranges ~init:0 ~f:(fun acc cur ->
      let start_range, end_range = cur in
      sum_in_range start_range end_range + acc)
  ;;

  let run () = Util.parse_file parse_ranges "day2.txt" |> sum_ranges
end

module Part2 = struct
  let get_multiples number =
    List.range ~stop:`inclusive 2 number
    |> List.filter ~f:(fun e -> number % e = 0)
  ;;

  let segment str segments =
    let len = String.length str in
    let rec loop (segmented_string : string list) current_segment =
      match current_segment <= segments with
      | false -> segmented_string
      | true ->
        let start = len / segments * (current_segment - 1) in
        let end_ = len / segments * current_segment in
        loop
          (String.slice str start end_ :: segmented_string)
          (current_segment + 1)
    in
    loop [] 1
  ;;

  let has_repeated_digits number =
    let number = string_of_int number in
    (* don't use 1 because the string wouldn't repeat *)
    let multiples =
      get_multiples (String.length number) |> List.filter ~f:(fun e -> e <> 1)
    in
    List.exists multiples ~f:(fun multiple ->
      let segmented_string = segment number multiple in
      List.for_all segmented_string ~f:(fun e ->
        String.( = ) e (List.hd_exn segmented_string)))
  ;;

  let sum_in_range range_start range_end =
    let rec loop current sum =
      match current <= range_end, has_repeated_digits current with
      | true, true -> loop (current + 1) sum + current
      | true, false -> loop (current + 1) sum
      | false, _ -> sum
    in
    loop range_start 0
  ;;

  let sum_ranges ranges =
    List.fold ranges ~init:0 ~f:(fun acc cur ->
      let start_range, end_range = cur in
      sum_in_range start_range end_range + acc)
  ;;

  let run () = Util.parse_file parse_ranges "day2.txt" |> sum_ranges
end
