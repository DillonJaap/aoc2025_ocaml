open Core
open Angstrom

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let parse_cephalopod_math =
  let skip_space = skip_many (char ' ') in
  let parse_int = take_while1 is_digit >>| int_of_string in
  let parse_operands = many1 (skip_space *> parse_int <* skip_space) in
  let parse_operators = many1 (choice [ char '*'; char '+' ] <* skip_space) in
  let* operands = sep_by1 (char '\n') parse_operands <* end_of_line in
  let* operators = parse_operators <* end_of_line <* end_of_input in
  return
    (operands
     |> List.transpose_exn
     |> List.map2_exn operators ~f:(fun operands operator -> operands, operator)
    )
;;

module Part1 = struct
  let run () =
    let math = Util.parse_file parse_cephalopod_math "day6.txt" in
    List.fold math ~init:0 ~f:(fun acc cur ->
      let sum =
        match cur with
        | '*', n -> List.fold n ~init:1 ~f:(fun acc cur -> acc * cur)
        | '+', n -> List.fold n ~init:0 ~f:(fun acc cur -> acc + cur)
        | _, _ -> failwith "not a valid operator"
      in
      sum + acc)
  ;;
end

module Part2 = struct
  let string_is_spaces = String.for_all ~f:(fun cur -> Char.equal cur ' ')
  let char_is_not_space ch = not @@ Char.equal ch ' '

  let parse input =
    (* operators *)
    let operators =
      input
      |> String.split_lines
      |> List.last_exn
      |> String.rev (* reverse so we can read left-to-right *)
      |> String.to_list
      |> List.filter ~f:char_is_not_space
    in
    let operand_lines =
      input
      |> String.split_lines
      (* remove operator line *)
      |> List.drop_last_exn
      (* reverse the list and transpose it so we can parse it normaly *)
      (* left-to-right top-to-bottom *)
      |> List.map ~f:(fun cur -> cur |> String.rev |> String.to_list)
      |> List.transpose_exn
      (* covert back to a string list *)
      |> List.map ~f:(fun cur -> cur |> String.of_char_list)
    in
    (* parse the lines - put the operands into (int list list) groupings *)
    let rec loop current_group groupings lines =
      match lines with
      (*  a line of all spaces is our "delimiter" operand groups *)
      | hd :: tl when string_is_spaces hd ->
        loop [] (current_group :: groupings) tl
      | hd :: tl ->
        let num = String.filter hd ~f:char_is_not_space |> int_of_string in
        loop (num :: current_group) groupings tl
      | [] -> List.rev (current_group :: groupings)
    in
    let operands = loop [] [] operand_lines in
    (* group the operands with their corresponding operators *)
    List.map2_exn operators operands ~f:(fun operands operator ->
      operands, operator)
  ;;

  let run () =
    let math = In_channel.read_all "inputs/day6.txt" |> parse in
    List.fold math ~init:0 ~f:(fun acc cur ->
      let sum =
        match cur with
        | '*', n -> List.fold n ~init:1 ~f:(fun acc cur -> acc * cur)
        | '+', n -> List.fold n ~init:0 ~f:(fun acc cur -> acc + cur)
        | _, _ -> failwith "not a valid operator"
      in
      sum + acc)
  ;;
end
