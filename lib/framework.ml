open! Core

module type Part = sig
  type t
  type input

  val run : input -> t
  val show : t -> unit
end

module type S = sig
  module Input : sig
    type t

    val load : In_channel.t -> t
  end

  module Part_1 : Part with type input := Input.t
  module Part_2 : Part with type input := Input.t
end

module Unimplemented = struct
  type t = Nothing.t

  let run _input = raise_s [%message "Unimplemented"]
  let show = Nothing.unreachable_code
end

type t = (module S) Int.Table.t

let registry : t = Int.Table.create ()
let register ~day impl = Int.Table.add_exn registry ~key:day ~data:impl

let command () =
  Hashtbl.to_alist registry
  |> List.map ~f:(fun (day, (module Implementation)) ->
         let command =
           Command.basic ~summary:[%string "Day %{day#Int}"]
           @@ let%map_open.Command input =
                anon
                  (maybe_with_default
                     [%string "input/%{day#Int}"]
                     ("file" %: Filename_unix.arg_type))
              and part = flag "-part" (required int) ~doc:"<1|2> part" in
              fun () ->
                let file = In_channel.create input in
                let input = Implementation.Input.load file in
                let module Part_sig = struct
                  module type S = Part with type input := Implementation.Input.t
                end
                in
                let (module Part : Part_sig.S) =
                  match part with
                  | 1 -> (module Implementation.Part_1)
                  | 2 -> (module Implementation.Part_2)
                  | part -> raise_s [%message "Only part 1/2 are allowed" (part : int)]
                in
                Part.run input |> Part.show
         in
         Int.to_string day, command)
  |> Command.group ~summary:"Advent of code"
;;
