open Core
open Angstrom

let parse_file parser file =
  let content = In_channel.read_all ("inputs/" ^ file) in
  match parse_string ~consume:All parser content with
  | Ok v -> v
  | Error msg -> failwith msg
;;
