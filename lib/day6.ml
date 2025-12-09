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
    print_s ([%sexp_of: (char * int list) list] math);
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
  let parse_right_to_left input =
    let list = input |> String.split_lines |> List.drop_last_exn in
    let operator_line = List.last_exn list in
    let operand_lines = List.drop_last_exn list in
    let rec loop i list =
      if i < (List.hd_exn operand_lines |> String.length)
      then (
        let str =
          List.fold operand_lines ~init:"" ~f:(fun acc cur ->
            (Char.to_string @@ String.get cur i) ^ acc)
        in
        loop (i + 1) (str :: list))
      else list
    in
    let operands = loop 0 [] |> List.map ~f:(fun a -> int_of_string a) in
    let operators =
      String.split operator_line ~on:' '
      |> List.filter ~f:(fun a -> String.( = ) a "")
    in
    |> List.map2_exn operands operators ~f:(fun operands operator -> operands, operator)
  ;;

  let run () =
    let math = Util.parse_file parse_cephalopod_math "day6.txt" in
    print_s ([%sexp_of: (char * int list) list] math);
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
