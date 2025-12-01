open Core
open Angstrom

type direction =
  | Left
  | Right
[@@deriving sexp_of]

(* Parsing *)
let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let get_turns =
  let get_turn =
    let* dir = char 'L' *> return Left <|> char 'R' *> return Right in
    let* num = take_while1 is_digit >>| int_of_string in
    let* _ = skip (fun a -> Char.( = ) a '\n') in
    return (dir, num)
  in
  many get_turn <* end_of_input
;;

(* Calculating *)
let do_turn current_position direction amount =
  let amount =
    match direction with
    | Left -> amount * -1
    | Right -> amount
  in
  (current_position + amount) % 100
;;

let get_code turns =
  let _, total =
    (* the Dial starts at 50 *)
    List.fold turns ~init:(50, 0) ~f:(fun acc cur ->
      let current_position, total = acc in
      let direction, turn_amount = cur in
      let new_position = do_turn current_position direction turn_amount in
      match new_position with
      | 0 -> new_position, total + 1
      | _ -> new_position, total)
  in
  total
;;

(* running *)
let part1 () =
  let content = In_channel.read_all "inputs/day1.txt" in
  let turns =
    match parse_string ~consume:All get_turns content with
    | Ok v -> v
    | Error msg -> failwith msg
  in
  (* print_s ([%sexp_of: (direction * int) list] turns) *)
  get_code turns
;;
