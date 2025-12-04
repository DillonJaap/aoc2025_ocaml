open Core

let parse_paper =
  In_channel.read_all "inputs/day4.txt"
  |> String.split ~on:'\n'
  |> List.drop_last_exn
  |> List.to_array
  |> Array.map ~f:(fun s -> String.to_array s)
;;

let print_grid grid =
  Array.iter grid ~f:(fun a ->
    Array.iter a ~f:(fun b -> printf "%c" b);
    print_endline "")
;;

module Part1 = struct
  let number_of_adjacent_rolls grid x y =
    (* printf "xlen: %d, ylen: %d\n" (Array.length grid) (Array.length grid.(0)); *)
    let get_roll x y =
      if
        x < 0
        || y < 0
        || x > Array.length grid - 1
        || y > Array.length grid.(0) - 1
      then 0
      else if Char.( = ) grid.(x).(y) '@'
      then 1
      else 0
    in
    get_roll (x - 1) y
    + get_roll x (y - 1)
    + get_roll (x - 1) (y - 1)
    + get_roll (x + 1) y
    + get_roll x (y + 1)
    + get_roll (x + 1) (y + 1)
    + get_roll (x + 1) (y - 1)
    + get_roll (x - 1) (y + 1)
  ;;

  let run () =
    let grid = parse_paper in
    Array.foldi grid ~init:0 ~f:(fun x total cur ->
      Array.foldi cur ~init:0 ~f:(fun y row_total _ ->
        (if Char.( = ) grid.(x).(y) '@' && number_of_adjacent_rolls grid x y < 4
         then 1
         else 0)
        + row_total)
      + total)
  ;;
end

module Part2 = struct
  let number_of_adjacent_rolls grid x y =
    (* printf "xlen: %d, ylen: %d\n" (Array.length grid) (Array.length grid.(0)); *)
    let get_roll x y =
      if
        x < 0
        || y < 0
        || x > Array.length grid - 1
        || y > Array.length grid.(0) - 1
      then 0
      else if Char.( = ) grid.(x).(y) '@'
      then 1
      else 0
    in
    get_roll (x - 1) y
    + get_roll x (y - 1)
    + get_roll (x - 1) (y - 1)
    + get_roll (x + 1) y
    + get_roll x (y + 1)
    + get_roll (x + 1) (y + 1)
    + get_roll (x + 1) (y - 1)
    + get_roll (x - 1) (y + 1)
  ;;

  let run () =
    let grid = parse_paper in
    let total = ref 0 in
    let some_left = ref true in
    let remove_rolls () =
      some_left := false;
      for x = 0 to Array.length grid - 1 do
        for y = 0 to Array.length grid.(0) - 1 do
          if
            Char.( = ) grid.(x).(y) '@' && number_of_adjacent_rolls grid x y < 4
          then (
            grid.(x).(y) <- '.';
            some_left := true;
            total := !total + 1)
          else ()
        done
      done
    in
    while !some_left do
      remove_rolls ()
    done;
    !total
  ;;
end
