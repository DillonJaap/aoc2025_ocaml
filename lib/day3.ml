open Core

(* Parsing  *)
let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

(* let parse_batteries = *)
(*   let* res = sep_by (char '\n') (take_while1 is_digit) in *)
(*   return *)
(*     (List.map res ~f:(fun s -> *)
(*        String.to_list s |> List.map ~f:(fun c -> int_of_char c))) *)
(* ;; *)

let parse_batteries =
  In_channel.read_all "inputs/day3.txt"
  |> String.split ~on:'\n'
  |> List.map ~f:(fun s ->
    String.to_list s
    |> List.map ~f:(fun c -> c |> Char.to_string |> int_of_string))
  |> List.drop_last_exn
;;

module Part1 = struct
  let find_maxi battery_bank =
    List.foldi battery_bank ~init:(0, 0) ~f:(fun i acc cur ->
      let max, _ = acc in
      match cur with
      | c when c > max -> c, i
      | _ -> acc)
  ;;

  let run () =
    parse_batteries
    |> List.map ~f:(fun battery_bank ->
      let max1, max1_index = find_maxi (List.drop_last_exn battery_bank) in
      let _, after_max_list = List.split_n battery_bank (max1_index + 1) in
      let max2, _ = find_maxi after_max_list in
      (max1 * 10) + max2)
    |> List.sum (module Int) ~f:(fun a -> a)
  ;;
end

module Part2 = struct
  let find_max_digit battery_bank digits =
    List.foldi battery_bank ~init:(0, 0) ~f:(fun i acc cur ->
      let max, _ = acc in
      match cur, i with
      | _, i when i > List.length battery_bank - digits -> acc
      | c, _ when c > max -> c, i
      | _, _ -> acc)
  ;;

  let find_max_n_digits battery_bank n =
    let rec loop cur_list cur_digit digit_list =
      match cur_digit with
      | 0 -> digit_list
      | _ ->
        let max_digit, index = find_max_digit cur_list cur_digit in
        let _, after_max_list = List.split_n cur_list (index + 1) in
        loop after_max_list (cur_digit - 1) (max_digit :: digit_list)
    in
    loop battery_bank n []
  ;;

  let run () =
    parse_batteries
    |> List.map ~f:(fun battery_bank ->
      let res, _ =
        find_max_n_digits battery_bank 12
        |> List.fold ~init:(0, 0) ~f:(fun acc cur ->
          let sum, power = acc in
          let sum = sum + (cur * Int.pow 10 power) in
          sum, power + 1)
      in
      res)
    |> List.sum (module Int) ~f:(fun a -> a)
  ;;
end
