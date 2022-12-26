open! Core

module Int_result = struct
  type t = int

  let show i = print_endline (Int.to_string i)
end

module Day_1 = struct
  module Input = struct
    type t = int list list

    let load in_channel =
      In_channel.input_lines in_channel
      |> List.map ~f:(fun s -> Option.try_with (fun () -> Int.of_string s))
      |> List.group ~break:(fun o1 o2 ->
             match o1, o2 with
             | Some _, Some _ | None, None -> false
             | Some _, None | None, Some _ -> true)
      |> List.filter_map ~f:Option.all
    ;;
  end

  module Part_1 = struct
    include Int_result

    let run input =
      List.map input ~f:(List.sum (module Int) ~f:Fn.id)
      |> List.max_elt ~compare
      |> Option.value_exn
    ;;
  end

  module Part_2 = struct
    include Int_result

    let run input =
      List.take
        (List.map input ~f:(List.sum (module Int) ~f:Fn.id)
        |> List.sort ~compare:(Comparable.reverse compare))
        3
      |> List.sum (module Int) ~f:Fn.id
    ;;
  end
end

let () = Framework.register ~day:1 (module Day_1)

module Day_2 = struct
  module Game = struct
    type t =
      | Rock
      | Paper
      | Scissors

    let outcome = function
      | Rock, Scissors | Paper, Rock | Scissors, Paper -> `Player_1
      | Scissors, Rock | Rock, Paper | Paper, Scissors -> `Player_2
      | Scissors, Scissors | Rock, Rock | Paper, Paper -> `Draw
    ;;

    let of_part1_interpretation = function
      | `A | `X -> Rock
      | `B | `Y -> Paper
      | `C | `Z -> Scissors
    ;;

    let score_player_2 game =
      let choice_score =
        match snd game with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3
      in
      let outcome_score =
        match outcome game with
        | `Player_2 -> 6
        | `Player_1 -> 0
        | `Draw -> 3
      in
      choice_score + outcome_score
    ;;

    let of_part1 (a, x) = of_part1_interpretation a, of_part1_interpretation x

    let of_part2 (a, x) =
      let player_1 = of_part1_interpretation a in
      let player_2 =
        (* X = lose, Y = draw, Z = win *)
        match x, player_1 with
        | `X, Scissors | `Y, Paper | `Z, Rock -> Paper
        | `X, Paper | `Y, Rock | `Z, Scissors -> Rock
        | `X, Rock | `Y, Scissors | `Z, Paper -> Scissors
      in
      player_1, player_2
    ;;
  end

  module Input = struct
    type game = [ `A | `B | `C ] * [ `X | `Y | `Z ] [@@deriving of_sexp]
    type t = game list

    let load in_channel =
      In_channel.input_lines in_channel
      |> List.map ~f:(fun line ->
             Sexp.of_string_conv_exn [%string "(%{line})"] game_of_sexp)
    ;;
  end

  module Part_1 = struct
    include Int_result

    let run input =
      List.map input ~f:Game.of_part1 |> List.sum (module Int) ~f:Game.score_player_2
    ;;
  end

  module Part_2 = struct
    include Int_result

    let run input =
      List.map input ~f:Game.of_part2 |> List.sum (module Int) ~f:Game.score_player_2
    ;;
  end
end

let () = Framework.register ~day:2 (module Day_2)

module Day_3 = struct
  module Input = struct
    type t = char list list

    let load in_channel = In_channel.input_lines in_channel |> List.map ~f:String.to_list
  end

  let priority = function
    | 'a' .. 'z' as c -> Char.to_int c - Char.to_int 'a' + 1
    | 'A' .. 'Z' as c -> Char.to_int c - Char.to_int 'A' + 27
    | c -> raise_s [%message "Bad character" (c : char)]
  ;;

  module Part_1 = struct
    include Int_result

    let split_compartments l =
      let a, b = List.split_n l (List.length l / 2) in
      Char.Set.of_list a, Char.Set.of_list b
    ;;

    let run input =
      List.sum
        (module Int)
        input
        ~f:(fun rucksack ->
          let c1, c2 = split_compartments rucksack in
          Set.inter c1 c2 |> Set.to_list |> List.sum (module Int) ~f:priority)
    ;;
  end

  module Part_2 = struct
    include Int_result

    let run input =
      List.groupi input ~break:(fun i _ _ -> i mod 3 = 0)
      |> List.sum
           (module Int)
           ~f:(fun group ->
             List.map group ~f:Char.Set.of_list
             |> List.reduce_exn ~f:Set.inter
             |> Set.to_list
             |> List.sum (module Int) ~f:priority)
    ;;
  end
end

let () = Framework.register ~day:3 (module Day_3)

module Day_4 = struct
  module Range = struct
    type t = int * int

    let of_string s =
      let a, b = String.lsplit2_exn s ~on:'-' in
      Int.of_string a, Int.of_string b
    ;;

    let contains ~larger ~smaller = fst larger <= fst smaller && snd larger >= snd smaller
    let overlap a b = not (snd a < fst b || snd b < fst a)
  end

  module Input = struct
    type t = (Range.t * Range.t) list

    let load in_channel =
      In_channel.input_lines in_channel
      |> List.map ~f:(fun s ->
             let a, b = String.lsplit2_exn s ~on:',' in
             Range.of_string a, Range.of_string b)
    ;;
  end

  module Part_1 = struct
    include Int_result

    let run ranges =
      List.count ranges ~f:(fun (a, b) ->
          Range.contains ~larger:a ~smaller:b || Range.contains ~larger:b ~smaller:a)
    ;;
  end

  module Part_2 = struct
    include Int_result

    let run ranges = List.count ranges ~f:(fun (a, b) -> Range.overlap a b)
  end
end

let () = Framework.register ~day:4 (module Day_4)

module Day_5 = struct
  module Input = struct
    module State = struct
      type t = char list (* stack *) Int.Map.t [@@deriving sexp_of]
    end

    module Action = struct
      type t =
        { count : int
        ; src : int
        ; dst : int
        }
      [@@deriving sexp_of]
    end

    type t = State.t * Action.t list [@@deriving sexp_of]

    module Parse = struct
      open! Angstrom

      let crate = char '[' *> satisfy Char.is_alpha <* char ']'
      let crate_or_none = lift Option.return crate <|> string "   " *> return None

      let crate_line =
        let%map.Angstrom crates = sep_by (char ' ') crate_or_none <* end_of_line in
        List.filter_mapi crates ~f:(fun i v ->
            let%map.Option v = v in
            i + 1, v)
      ;;

      let index_line =
        sep_by (char ' ') (char ' ' *> satisfy Char.is_digit <* char ' ') <* end_of_line
      ;;

      let state : State.t Angstrom.t =
        let%map.Angstrom state = many_till crate_line index_line in
        List.concat state |> Int.Map.of_alist_multi
      ;;

      let number = take_while1 Char.is_digit |> map ~f:Int.of_string

      let action =
        let%mapn.Angstrom count = string "move " *> number
        and src = string " from " *> number
        and dst = string " to " *> number in
        { Action.count; src; dst }
      ;;

      let parser =
        let%mapn.Angstrom state = state <* end_of_line
        and actions = sep_by end_of_line action <* end_of_line in
        state, actions
      ;;

      let parse s = Angstrom.parse_string ~consume:All parser s |> Result.ok_or_failwith
    end

    let load in_channel = In_channel.input_all in_channel |> Parse.parse
  end

  module Part_1 = struct
    type t = char list

    let apply (state : Input.State.t) { Input.Action.count; src; dst } =
      let existing_src = Map.find_multi state src in
      let moved_rev, remaining_src = List.split_n existing_src count in
      let state = Map.set state ~key:src ~data:remaining_src in
      Map.set state ~key:dst ~data:(Map.find_multi state dst |> List.rev_append moved_rev)
    ;;

    let run (state, actions) =
      List.fold ~init:state actions ~f:apply |> Map.data |> List.map ~f:List.hd_exn
    ;;

    let show x = String.of_char_list x |> print_endline
  end

  module Part_2 = struct
    type t = char list

    let apply (state : Input.State.t) { Input.Action.count; src; dst } =
      let existing_src = Map.find_multi state src in
      let moved, remaining_src = List.split_n existing_src count in
      let state = Map.set state ~key:src ~data:remaining_src in
      Map.set state ~key:dst ~data:(Map.find_multi state dst |> List.append moved)
    ;;

    let run (state, actions) =
      List.fold ~init:state actions ~f:apply |> Map.data |> List.map ~f:List.hd_exn
    ;;

    let show x = String.of_char_list x |> print_endline
  end
end

let () = Framework.register ~day:5 (module Day_5)

module Day_6 = struct
  module Input = struct
    type t = string

    let load in_channel = In_channel.input_all in_channel
  end

  let run ~length s =
    String.fold_until
      s
      ~init:(1, Queue.create ())
      ~f:(fun (i, queue) c ->
        Queue.enqueue queue c;
        if Queue.length queue > length then ignore (Queue.dequeue_exn queue : char);
        if Queue.to_list queue |> Char.Set.of_list |> Set.length |> ( = ) length
        then Stop i
        else Continue (i + 1, queue))
      ~finish:(fun _ -> assert false)
  ;;

  module Part_1 = struct
    include Int_result

    let run = run ~length:4
  end

  module Part_2 = struct
    include Int_result

    let run = run ~length:14
  end
end

let () = Framework.register ~day:6 (module Day_6)
let link () = ()
