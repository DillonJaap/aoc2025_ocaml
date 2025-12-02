open Core
open Angstrom

(* types  *)
type direction =
  | Left
  | Right
[@@deriving sexp_of]

let sign_of dir =
  match dir with
  | Left -> -1
  | Right -> 1
;;

(* Parsing *)
let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let parse_turns =
  let get_turn =
    let* dir = char 'L' *> return Left <|> char 'R' *> return Right in
    let* num = take_while1 is_digit >>| int_of_string in
    let* _ = skip (fun a -> Char.( = ) a '\n') in
    return (dir, num)
  in
  many get_turn <* end_of_input
;;

module Part1 = struct
  let do_turn current_position amount = (current_position + amount) % 100

  let get_code turns =
    let _, total =
      (* the Dial starts at 50 *)
      List.fold turns ~init:(50, 0) ~f:(fun acc cur ->
        let current_position, total = acc in
        let direction, turn_amount = cur in
        let turn_amount = turn_amount * sign_of direction in
        let new_position = do_turn current_position turn_amount in
        match new_position with
        | 0 -> new_position, total + 1
        | _ -> new_position, total)
    in
    total
  ;;

  (* running *)
  let run () = Util.parse_file parse_turns "day1.txt" |> get_code
end

module Part2 = struct
  let do_turn current_position turn_amount =
    (current_position + turn_amount) % 100
  ;;

  let number_of_times_crossed_zero current_position turn_amount =
    let new_position = current_position + turn_amount in
    match new_position, current_position with
    (* don't add 1 if we start on zero it didn't cross zero, it moved off of zero *)
    | p, 0 when p <= 0 -> Int.abs p / 100
    | p, _ when p <= 0 -> (Int.abs p / 100) + 1
    | p, _ -> p / 100
  ;;

  let get_code turns =
    let _, total =
      (* the Dial starts at 50 *)
      List.fold turns ~init:(50, 0) ~f:(fun acc cur ->
        let current_position, total = acc in
        let direction, turn_amount = cur in
        let turn_amount = turn_amount * sign_of direction in
        let total =
          total + number_of_times_crossed_zero current_position turn_amount
        in
        let new_position = do_turn current_position turn_amount in
        new_position, total)
    in
    total
  ;;

  let run () = Util.parse_file parse_turns "day1.txt" |> get_code
end
