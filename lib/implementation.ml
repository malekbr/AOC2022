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

module Day_7 = struct
  module Input = struct
    module Cd = struct
      type t =
        | Root
        | Dir of string
        | Parent
      [@@deriving sexp_of]
    end

    module Ls = struct
      type t =
        | Dir of string
        | File of
            { size : int
            ; name : string
            }
      [@@deriving sexp_of]
    end

    module Command = struct
      type t =
        | Cd of Cd.t
        | Ls of Ls.t list
      [@@deriving sexp_of]
    end

    type t = Command.t list [@@deriving sexp_of]

    module Parse = struct
      open! Angstrom

      let root = char '/'
      let parent = string ".."
      let file = take_while1 (Fn.non Char.is_whitespace)

      let cd =
        let%map.Angstrom cd =
          string "$ cd "
          *> choice
               [ root *> return Cd.Root
               ; parent *> return Cd.Parent
               ; map file ~f:(fun dir -> Cd.Dir dir)
               ]
        in
        Command.Cd cd
      ;;

      let ls_dir = string "dir " *> file |> map ~f:(fun file -> Ls.Dir file)

      let ls_file =
        let%mapn.Angstrom size =
          map ~f:Int.of_string (take_while1 Char.is_alphanum) <* char ' '
        and name = file in
        Ls.File { size; name }
      ;;

      let ls =
        string "$ ls" *> many (end_of_line *> (ls_dir <|> ls_file))
        |> map ~f:(fun ls -> Command.Ls ls)
      ;;

      let parser = many (cd <|> ls <* end_of_line)
      let parse s = Angstrom.parse_string ~consume:All parser s |> Result.ok_or_failwith
    end

    let load in_channel = In_channel.input_all in_channel |> Parse.parse
  end

  module Structure = struct
    type 'a t =
      | Dir of
          { files : 'a t String.Map.t
          ; data : 'a
          }
      | File of { size : int }
    [@@deriving sexp_of]

    let go_parent parent_trail pwd =
      match parent_trail with
      | [] -> None
      | (name, parent) :: rest ->
        let pwd = Map.set parent ~key:name ~data:(Dir { files = pwd; data = () }) in
        Some (rest, pwd)
    ;;

    let rec go_to_root parent_trail pwd =
      match go_parent parent_trail pwd with
      | None -> pwd
      | Some (trail, pwd) -> go_to_root trail pwd
    ;;

    let rec build parent_trail pwd = function
      | [] -> go_to_root parent_trail pwd
      | Input.Command.Cd cd :: rest ->
        (match cd with
        | Root ->
          let root = go_to_root parent_trail pwd in
          build [] root rest
        | Parent ->
          let trail, pwd = go_parent parent_trail pwd |> Option.value_exn in
          build trail pwd rest
        | Dir dir_name ->
          let dir_content =
            match Map.find_exn pwd dir_name with
            | Dir { files; data = () } -> files
            | File _ -> raise_s [%message "Is file" (dir_name : string)]
          in
          build ((dir_name, pwd) :: parent_trail) dir_content rest)
      | Input.Command.Ls ls :: rest ->
        if Map.is_empty pwd
        then (
          let pwd =
            List.map ls ~f:(function
                | File { size; name } -> name, File { size }
                | Dir name -> name, Dir { files = String.Map.empty; data = () })
            |> String.Map.of_alist_exn
          in
          build parent_trail pwd rest)
        else (* We already visited here before *) build parent_trail pwd rest
    ;;

    let build commands = Dir { files = build [] String.Map.empty commands; data = () }

    let rec bind t ~f =
      match t with
      | File { size } -> File { size }
      | Dir { files; data } -> f ~files:(Map.map files ~f:(bind ~f)) ~data
    ;;

    let total_size t =
      bind t ~f:(fun ~files ~data:() ->
          let data =
            Map.data files
            |> List.sum
                 (module Int)
                 ~f:(fun (File { size } | Dir { files = _; data = size }) -> size)
          in
          Dir { files; data })
    ;;

    let rec fold_dir t ~init ~f =
      match t with
      | Dir { files; data } ->
        Map.fold files ~init:(f init data) ~f:(fun ~key:_ ~data init ->
            fold_dir data ~init ~f)
      | File _ -> init
    ;;
  end

  module Part_1 = struct
    include Int_result

    let run commands =
      Structure.build commands
      |> Structure.total_size
      |> Structure.fold_dir ~init:0 ~f:(fun total size ->
             if size <= 100_000 then size + total else total)
    ;;
  end

  module Part_2 = struct
    include Int_result

    let run commands =
      let system_size = 70000000 in
      let required_free = 30000000 in
      let data = Structure.build commands |> Structure.total_size in
      let current_used =
        match data with
        | Dir { data; _ } -> data
        | File _ -> assert false
      in
      let current_free = system_size - current_used in
      let needed_delete = required_free - current_free in
      Structure.fold_dir data ~init:None ~f:(fun size_to_delete size ->
          if size >= needed_delete
          then (
            match size_to_delete with
            | None -> Some size
            | Some existing -> Some (Int.min size existing))
          else size_to_delete)
      |> Option.value_exn
    ;;
  end
end

let () = Framework.register ~day:7 (module Day_7)

module Day_8 = struct
  module Input = struct
    type t = int array array

    let load in_channel =
      In_channel.input_lines in_channel
      |> Array.of_list_map ~f:(fun string ->
             String.to_array string
             |> Array.map ~f:(fun char -> Char.to_int char - Char.to_int '0'))
    ;;
  end

  module Visibility_map = struct
    type 'a t = 'a array array [@@deriving sexp_of]

    let rows t = Array.length t
    let cols t = Array.length t.(0)

    module Iterator = struct
      module Dimension = struct
        type t =
          { current : int
          ; reset : int
          ; stop_condition : int
          ; direction : int
          }

        let next t =
          let next = t.current + t.direction in
          if next = t.stop_condition
          then `Reset { t with current = t.reset }
          else `Run { t with current = next }
        ;;
      end

      type ('r, 'c) t =
        | Base : (Nothing.t, Nothing.t) t
        | Row : Dimension.t * (Nothing.t, 'c) t -> (int, 'c) t
        | Column : Dimension.t * ('r, Nothing.t) t -> ('r, int) t

      let compute outer inner =
        match Dimension.next inner with
        | `Run inner -> outer, inner, `Take
        | `Reset inner ->
          (match Dimension.next outer with
          | `Run outer -> outer, inner, `Reset
          | `Reset outer -> outer, inner, `Stop)
      ;;

      let next = function
        | Row (row, Column (column, Base)) ->
          let yield = row.current, column.current in
          let row, column, action = compute row column in
          Row (row, Column (column, Base)), yield, action
        | Column (column, Row (row, Base)) ->
          let yield = row.current, column.current in
          let column, row, action = compute column row in
          Column (column, Row (row, Base)), yield, action
      ;;
    end

    let fold_map_1d_iterator ~temp (input : Input.t) iterator ~init ~f =
      let t = Array.map input ~f:(Array.map ~f:(const temp)) in
      let rec go iterator ~acc =
        let iterator, (r, c), action = Iterator.next iterator in
        let acc, value = f acc input.(r).(c) in
        t.(r).(c) <- value;
        match action with
        | `Take -> go iterator ~acc
        | `Reset -> go iterator ~acc:init
        | `Stop -> ()
      in
      go iterator ~acc:init;
      t
    ;;

    let compute_visibility input iterator =
      fold_map_1d_iterator
        input
        iterator
        ~temp:false
        ~init:(-1)
        ~f:(fun current_height tree_height ->
          Int.max current_height tree_height, current_height < tree_height)
    ;;

    let compute_viewing_distance input iterator =
      fold_map_1d_iterator
        ~temp:0
        input
        iterator
        ~init:[]
        ~f:(fun heights_and_count tree_height ->
          let rec compute smaller_tree_count = function
            | [] -> [ tree_height, 1 + smaller_tree_count ], smaller_tree_count
            | (height, existing_count) :: rest as all ->
              if tree_height < height
              then
                (tree_height, 1 + smaller_tree_count) :: all, smaller_tree_count + 1
                (* Self + blocking tree + smaller trees *)
              else if tree_height = height
              then
                ( (height, existing_count + smaller_tree_count + 1) :: rest
                , smaller_tree_count + 1 )
              else compute (existing_count + smaller_tree_count) rest
          in
          compute 0 heights_and_count)
    ;;

    let forward ~f t =
      { Iterator.Dimension.current = 0; reset = 0; stop_condition = f t; direction = 1 }
    ;;

    let backward ~f t =
      let reset = f t - 1 in
      { Iterator.Dimension.current = reset; reset; stop_condition = -1; direction = -1 }
    ;;

    let top_down t = Iterator.Column (forward ~f:cols t, Row (forward ~f:rows t, Base))
    let bottom_up t = Iterator.Column (forward ~f:cols t, Row (backward ~f:rows t, Base))
    let left_right t = Iterator.Row (forward ~f:rows t, Column (forward ~f:cols t, Base))
    let right_left t = Iterator.Row (forward ~f:rows t, Column (backward ~f:cols t, Base))
    let ( ||* ) = Array.map2_exn ~f:(Array.map2_exn ~f:( || ))
    let ( *. ) = Array.map2_exn ~f:(Array.map2_exn ~f:( * ))
    let count = Array.sum (module Int) ~f:(Array.count ~f:Fn.id)

    let max t =
      Array.map t ~f:(fun a -> Array.max_elt ~compare a |> Option.value_exn)
      |> Array.max_elt ~compare
      |> Option.value_exn
    ;;
  end

  module Part_1 = struct
    include Int_result

    let run input =
      Visibility_map.[ top_down; bottom_up; left_right; right_left ]
      |> List.map ~f:(fun iterator ->
             Visibility_map.compute_visibility input (iterator input))
      |> List.reduce_exn ~f:Visibility_map.( ||* )
      |> Visibility_map.count
    ;;
  end

  module Part_2 = struct
    include Int_result

    let run input =
      Visibility_map.[ top_down; bottom_up; left_right; right_left ]
      |> List.map ~f:(fun iterator ->
             Visibility_map.compute_viewing_distance input (iterator input))
      |> List.reduce_exn ~f:Visibility_map.( *. )
      |> Visibility_map.max
    ;;
  end
end

let () = Framework.register ~day:8 (module Day_8)

module Day_9 = struct
  module Dir = struct
    type t =
      | Right
      | Up
      | Left
      | Down

    let of_char = function
      | 'R' -> Right
      | 'U' -> Up
      | 'L' -> Left
      | 'D' -> Down
      | c -> raise_s [%message "Bad character" (c : char)]
    ;;
  end

  module Input = struct
    type t = (Dir.t * int) list

    let load in_channel =
      In_channel.input_lines in_channel
      |> List.map ~f:(fun s ->
             Scanf.sscanf s "%c %d" (fun dir count -> Dir.of_char dir, count))
    ;;
  end

  module Simulation = struct
    module Pos = struct
      module T = struct
        type t = int * int [@@deriving sexp_of, compare]
      end

      include T
      include Comparable.Make_plain (T)
    end

    type t =
      { rope : Pos.t list
      ; visited_tail : Pos.Set.t
      }

    let init n =
      let pos = 0, 0 in
      { rope = List.init n ~f:(const pos); visited_tail = Pos.Set.singleton pos }
    ;;

    let move_head (r, c) direction =
      match direction with
      | Dir.Right -> r, c + 1
      | Left -> r, c - 1
      | Up -> r - 1, c
      | Down -> r + 1, c
    ;;

    let adjust_tail ~head:(rh, ch) ~tail:(rt, ct) =
      let classify = function
        | (-1 | 1) as n -> `Adjacent n
        | 0 -> `Same 0
        | n -> if n < 0 then `Far (-1) else `Far 1
      in
      let dr, dc =
        match classify (rh - rt), classify (ch - ct) with
        | (`Adjacent _ | `Same _), (`Adjacent _ | `Same _) -> 0, 0
        | `Far r, `Same c
        | `Same r, `Far c
        | `Far r, `Far c
        | `Adjacent r, `Far c
        | `Far r, `Adjacent c -> r, c
      in
      rt + dr, ct + dc
    ;;

    let adjust_rope rope direction =
      let head, rest =
        match rope with
        | head :: rest -> head, rest
        | _ -> raise_s [%message "Rope needs to have at least one element"]
      in
      let rec go ~acc = function
        | [] -> List.rev acc, List.hd_exn acc
        | knot :: rest ->
          go ~acc:(adjust_tail ~head:(List.hd_exn acc) ~tail:knot :: acc) rest
      in
      go ~acc:[ move_head head direction ] rest
    ;;

    let rec simulate t (direction, count) =
      if count <= 0
      then t
      else (
        let rope, tail = adjust_rope t.rope direction in
        let visited_tail = Set.add t.visited_tail tail in
        simulate { rope; visited_tail } (direction, count - 1))
    ;;
  end

  module Part_1 = struct
    include Int_result

    let run input =
      let simulation = List.fold input ~init:(Simulation.init 2) ~f:Simulation.simulate in
      Set.length simulation.visited_tail
    ;;
  end

  module Part_2 = struct
    include Int_result

    let run input =
      let simulation =
        List.fold input ~init:(Simulation.init 10) ~f:Simulation.simulate
      in
      Set.length simulation.visited_tail
    ;;
  end
end

let () = Framework.register ~day:9 (module Day_9)

module Day_10 = struct
  module Instruction = struct
    type t =
      | Noop
      | Addx of int
  end

  module Input = struct
    type t = Instruction.t list

    module Parse = struct
      open! Angstrom

      let noop = string "noop" *> return Instruction.Noop

      let addx =
        let%map.Angstrom v =
          string "addx " *> consumed (option '+' (char '-') *> take_while1 Char.is_digit)
        in
        Instruction.Addx (Int.of_string v)
      ;;

      let parser = many (noop <|> addx <* end_of_line)
      let parse s = Angstrom.parse_string ~consume:All parser s |> Result.ok_or_failwith
    end

    let load in_channel = In_channel.input_all in_channel |> Parse.parse
  end

  module Simulation = struct
    type t =
      { cycle : int
      ; x : int
      }

    let run t instruction =
      match instruction with
      | Instruction.Noop -> [ t ]
      | Instruction.Addx n -> [ t; t + n ]
    ;;

    let start = { cycle = 1; x = 1 }

    let fold_run instructions ~init ~f =
      List.fold
        instructions
        ~init:(start, f init start)
        ~f:(fun (t, init) instruction ->
          run t.x instruction
          |> List.fold ~init:(t, init) ~f:(fun (t, init) x ->
                 let t = { x; cycle = t.cycle + 1 } in
                 t, f init t))
    ;;
  end

  module Part_1 = struct
    include Int_result

    let run instructions =
      Simulation.fold_run instructions ~init:0 ~f:(fun count state ->
          if state.cycle mod 40 = 20 then (state.x * state.cycle) + count else count)
      |> snd
    ;;
  end

  module Part_2 = struct
    type t = string list

    let sprite_set x cycle = (cycle - x) % 40 < 3

    let%expect_test _ =
      let test x cycle = print_s [%sexp (sprite_set x cycle : bool)] in
      test 1 1;
      test 1 2;
      test 16 3;
      test 16 4;
      test 5 5;
      test 8 10;
      [%expect
        {|
        true
        true
        false
        false
        true
        true |}]
    ;;

    let run instructions =
      Simulation.fold_run instructions ~init:[] ~f:(fun output state ->
          if sprite_set state.x state.cycle then '#' :: output else '.' :: output)
      |> snd
      |> List.rev
      |> List.groupi ~break:(fun i _ _ -> i mod 40 = 0)
      |> List.map ~f:String.of_char_list
    ;;

    let show output = List.iter output ~f:print_endline
  end
end

let () = Framework.register ~day:10 (module Day_10)

module Day_11 = struct
  module Operand = struct
    type t =
      | Old
      | Value of int
  end

  module Operation = struct
    type t =
      | Add
      | Mul
  end

  module Expr = struct
    type t = Operation.t * Operand.t * Operand.t

    let evaluate (operation, left, right) ~old =
      let value = function
        | Operand.Old -> old
        | Value v -> v
      in
      let operation =
        match operation with
        | Operation.Add -> ( + )
        | Mul -> ( * )
      in
      operation (value left) (value right)
    ;;
  end

  module Monkey = struct
    type 'a t =
      { index : int
      ; items : 'a Queue.t
      ; operation : Expr.t
      ; divisible_by : int
      ; true_monkey : int
      ; false_monkey : int
      }

    let map t ~f = { t with items = Queue.map t.items ~f }
  end

  module Input = struct
    type t = int Monkey.t list

    module Parse = struct
      open! Angstrom

      let number = take_while1 Char.is_digit |> lift Int.of_string

      let operand =
        lift (fun v -> Operand.Value v) number <|> string "old" *> return Operand.Old
      ;;

      let operation =
        char '+' *> return Operation.Add <|> char '*' *> return Operation.Mul
      ;;

      let expr =
        let%mapn.Angstrom left = operand
        and operation = char ' ' *> operation <* char ' '
        and right = operand in
        operation, left, right
      ;;

      let monkey =
        let%map.Angstrom index = string "Monkey " *> number <* char ':' <* end_of_line
        and items =
          string "  Starting items: " *> sep_by1 (string ", ") number <* end_of_line
        and operation = string "  Operation: new = " *> expr <* end_of_line
        and divisible_by = string "  Test: divisible by " *> number <* end_of_line
        and true_monkey = string "    If true: throw to monkey " *> number <* end_of_line
        and false_monkey =
          string "    If false: throw to monkey " *> number <* end_of_line
        in
        { Monkey.index
        ; items = Queue.of_list items
        ; operation
        ; divisible_by
        ; true_monkey
        ; false_monkey
        }
      ;;

      let parser = sep_by1 end_of_line monkey
      let parse s = Angstrom.parse_string ~consume:All parser s |> Result.ok_or_failwith
    end

    let load in_channel = In_channel.input_all in_channel |> Parse.parse
  end

  module type S = sig
    type t

    val map : t -> f:(int -> int) -> t
    val evaluate : t -> int -> bool
  end

  let evaluate_monkey (type a) (module M : S with type t = a) (monkey : a Monkey.t) =
    let items = Queue.to_list monkey.items in
    Queue.clear monkey.items;
    List.map items ~f:(fun item ->
        let new_item = M.map item ~f:(fun old -> Expr.evaluate monkey.operation ~old) in
        let next_monkey =
          if M.evaluate new_item monkey.divisible_by
          then monkey.true_monkey
          else monkey.false_monkey
        in
        new_item, next_monkey)
  ;;

  let evaluate_monkeys field monkey_array count_array =
    Array.iteri monkey_array ~f:(fun i monkey ->
        let new_items = evaluate_monkey field monkey in
        count_array.(i) <- count_array.(i) + List.length new_items;
        List.iter new_items ~f:(fun (item, monkey) ->
            Queue.enqueue monkey_array.(monkey).items item))
  ;;

  module Part_1 = struct
    include Int_result

    module Field = struct
      type t = int

      let map t ~f = f t / 3
      let evaluate t n = t % n = 0
    end

    let run monkeys =
      let monkeys = Array.of_list monkeys in
      let counts = Array.map monkeys ~f:(const 0) in
      for _ = 1 to 20 do
        evaluate_monkeys (module Field) monkeys counts
      done;
      Array.sort counts ~compare:Int.descending;
      counts.(0) * counts.(1)
    ;;
  end

  module Part_2 = struct
    include Int_result

    module Field = struct
      type t = int Int.Map.t

      let make monkeys value =
        List.map monkeys ~f:(fun monkey ->
            let d = monkey.Monkey.divisible_by in
            d, value % d)
        |> Int.Map.of_alist_reduce ~f:(fun a _ -> a)
      ;;

      let map t ~f = Map.mapi t ~f:(fun ~key ~data -> f data % key)
      let evaluate t n = Map.find_exn t n = 0
    end

    let run monkeys =
      let monkeys = Array.of_list_map monkeys ~f:(Monkey.map ~f:(Field.make monkeys)) in
      let counts = Array.map monkeys ~f:(const 0) in
      for _ = 1 to 10_000 do
        evaluate_monkeys (module Field) monkeys counts
      done;
      Array.sort counts ~compare:Int.descending;
      counts.(0) * counts.(1)
    ;;
  end
end

let () = Framework.register ~day:11 (module Day_11)

module Day_12 = struct
  module Pos = struct
    module T = struct
      type t = int * int [@@deriving sexp_of, compare]
    end

    include T
    include Comparable.Make_plain (T)
  end

  module Input = struct
    type t =
      { elevations : int array array
      ; start : Pos.t
      ; goal : Pos.t
      }
    [@@deriving sexp_of]

    let load in_channel =
      let (start, goal), elevations =
        In_channel.input_lines in_channel
        |> Array.of_list
        |> Array.fold_mapi
             ~init:((0, 0), (0, 0))
             ~f:(fun r init s ->
               Array.fold_mapi ~init (String.to_array s) ~f:(fun c (start, goal) ->
                 function
                 | 'S' -> ((r, c), goal), 0
                 | 'E' -> (start, (r, c)), 25
                 | c -> (start, goal), Char.to_int c - Char.to_int 'a'))
      in
      { start; goal; elevations }
    ;;
  end

  let neighbors elevations (r, c) ~valid_neighbor =
    let value = elevations.(r).(c) in
    let check_neighbor (r, c) =
      match valid_neighbor ~to_:elevations.(r).(c) ~from:value with
      | true -> Some (r, c)
      | false | (exception _) -> None
    in
    List.filter_map [ r + 1, c; r - 1, c; r, c + 1; r, c - 1 ] ~f:check_neighbor
  ;;

  let search ~goal_reached ~valid_neighbor elevations start =
    let queue = Queue.create () in
    let explored = Pos.Set.singleton start in
    Queue.enqueue queue [ start ];
    let rec loop explored =
      match Queue.dequeue queue with
      | None ->
        Array.iteri elevations ~f:(fun r line ->
            Array.iteri line ~f:(fun c _ ->
                if Set.mem explored (r, c)
                then
                  Out_channel.output_char
                    stdout
                    (Char.of_int_exn (elevations.(r).(c) + Char.to_int 'a'))
                else Out_channel.output_char stdout ' ');
            Out_channel.newline stdout);
        raise_s [%message "No path found"]
      | Some path ->
        let pos = List.hd_exn path in
        if goal_reached pos
        then path
        else
          neighbors elevations pos ~valid_neighbor
          |> List.fold ~init:explored ~f:(fun explored pos ->
                 if Set.mem explored pos
                 then explored
                 else (
                   Queue.enqueue queue (pos :: path);
                   Set.add explored pos))
          |> loop
    in
    loop explored
  ;;

  module Part_1 = struct
    include Int_result

    let run { Input.elevations; start; goal } =
      (search
         elevations
         start
         ~goal_reached:([%compare.equal: Pos.t] goal)
         ~valid_neighbor:(fun ~to_ ~from -> to_ <= from + 1)
      |> List.length)
      - 1
    ;;
  end

  module Part_2 = struct
    include Int_result

    let run { Input.elevations; start = _; goal } =
      let path =
        search
          elevations
          goal
          ~goal_reached:(fun (r, c) -> elevations.(r).(c) = 0)
          ~valid_neighbor:(fun ~to_ ~from -> from <= to_ + 1)
      in
      List.length path - 1
    ;;
  end
end

let () = Framework.register ~day:12 (module Day_12)

module Day_13 = struct
  module Packet = struct
    type t =
      | List of t list
      | Value of int
    [@@deriving equal]

    let lower_divider = List [ List [ Value 2 ] ]
    let upper_divider = List [ List [ Value 6 ] ]

    let rec to_string = function
      | Value int -> Int.to_string int
      | List l -> List.map l ~f:to_string |> String.concat ~sep:"," |> sprintf "[%s]"
    ;;
  end

  module Input = struct
    type t = (Packet.t * Packet.t) list

    module Parse = struct
      open! Angstrom

      let number = take_while1 Char.is_digit |> map ~f:Int.of_string

      let packet =
        fix (fun packet ->
            choice
              [ char '[' *> sep_by (char ',') packet
                <* char ']'
                |> lift (fun packet -> Packet.List packet)
              ; lift (fun number -> Packet.Value number) number
              ])
      ;;

      let pair = both (packet <* end_of_line) (packet <* end_of_line)
      let parser = sep_by end_of_line pair
      let parse s = Angstrom.parse_string ~consume:All parser s |> Result.ok_or_failwith
    end

    let load in_channel = In_channel.input_all in_channel |> Parse.parse
  end

  let rec packet_compare packet_1 packet_2 =
    match packet_1, packet_2 with
    | Packet.Value x, Packet.Value y -> compare x y
    | List x, List y -> List.compare packet_compare x y
    | Value x, List y -> List.compare packet_compare [ Value x ] y
    | List x, Value y -> List.compare packet_compare x [ Value y ]
  ;;

  module Part_1 = struct
    include Int_result

    let run input =
      List.filter_mapi input ~f:(fun i (packet_1, packet_2) ->
          if packet_compare packet_1 packet_2 > 0 then None else Some (i + 1))
      |> List.sum (module Int) ~f:Fn.id
    ;;
  end

  module Part_2 = struct
    include Int_result

    let run input =
      List.concat_map
        ((Packet.lower_divider, Packet.upper_divider) :: input)
        ~f:(fun (packet_1, packet_2) -> [ packet_1; packet_2 ])
      |> List.sort ~compare:packet_compare
      |> List.filter_mapi ~f:(fun i packet ->
             if Packet.equal packet Packet.lower_divider
                || Packet.equal packet Packet.upper_divider
             then Some (i + 1)
             else None)
      |> List.fold ~init:1 ~f:( * )
    ;;
  end
end

let () = Framework.register ~day:13 (module Day_13)

module Day_14 = struct
  module Pos = struct
    module T = struct
      type t = int * int [@@deriving sexp_of, compare]
    end

    include T
    include Comparable.Make_plain (T)

    let ( - ) (a, b) (c, d) = a - c, b - d
    let ( + ) (a, b) (c, d) = a + c, b + d

    let normalize (a, b) =
      let open Int.O in
      let normalize n = if n > 0 then 1 else if n = 0 then 0 else -1 in
      normalize a, normalize b
    ;;
  end

  module Input = struct
    type t = Pos.t list list

    module Parse = struct
      open! Angstrom

      let number = take_while1 Char.is_digit |> map ~f:Int.of_string
      let pair : Pos.t t = both (number <* char ',') number
      let line = sep_by1 (string " -> ") pair
      let parser = many (line <* end_of_line)
      let parse s = Angstrom.parse_string ~consume:All parser s |> Result.ok_or_failwith
    end

    let load in_channel = In_channel.input_all in_channel |> Parse.parse
  end

  let rec fold_positions ~from ~to_ ~init ~f =
    let init = f init from in
    if [%compare.equal: Pos.t] from to_
    then init
    else fold_positions ~from:Pos.(normalize (to_ - from) + from) ~to_ ~init ~f
  ;;

  let rec fold_line ~init ~f = function
    | [ _ ] | [] -> init
    | from :: (to_ :: _ as rest) ->
      let init = fold_positions ~from ~to_ ~init ~f in
      fold_line ~init ~f rest
  ;;

  let fold_input (input : Input.t) ~init ~f =
    List.fold input ~init ~f:(fun init line -> fold_line ~init ~f line)
  ;;

  let build input = fold_input input ~init:Pos.Set.empty ~f:Set.add
  let lowest_point = Set.fold ~init:0 ~f:(fun v (_, y) -> max v y)

  let rec drop_ball_part_1 ~lowest_point set pos =
    if snd pos > lowest_point
    then `Full
    else (
      match
        List.find
          Pos.[ pos + (0, 1); pos + (-1, 1); pos + (1, 1) ]
          ~f:(fun pos -> not (Set.mem set pos))
      with
      | None -> `Stable (Set.add set pos)
      | Some pos -> drop_ball_part_1 ~lowest_point set pos)
  ;;

  let rec drop_ball_part_2 ~lowest_point set pos =
    match
      List.find
        Pos.[ pos + (0, 1); pos + (-1, 1); pos + (1, 1) ]
        ~f:(fun pos -> not (Set.mem set pos || snd pos = lowest_point + 2))
    with
    | None -> `Stable (Set.add set pos)
    | Some pos -> drop_ball_part_2 ~lowest_point set pos
  ;;

  let drop_ball ~f ~lowest_point set pos =
    if Set.mem set pos then `Full else f ~lowest_point set pos
  ;;

  let rec simulate ~count ~lowest_point ~f set =
    match drop_ball ~lowest_point set (500, 0) ~f with
    | `Full -> count
    | `Stable set -> simulate ~count:(count + 1) ~lowest_point set ~f
  ;;

  module Part_1 = struct
    include Int_result

    let run input =
      let grid = build input in
      let lowest_point = lowest_point grid in
      simulate ~count:0 ~lowest_point grid ~f:drop_ball_part_1
    ;;
  end

  module Part_2 = struct
    include Int_result

    let run input =
      let grid = build input in
      let lowest_point = lowest_point grid in
      simulate ~count:0 ~lowest_point grid ~f:drop_ball_part_2
    ;;
  end
end

let () = Framework.register ~day:14 (module Day_14)

module Day_15 = struct
  module Pos = struct
    module T = struct
      type t = int * int [@@deriving sexp_of, compare]
    end

    include T
    include Comparable.Make_plain (T)

    let ( - ) (a, b) (c, d) = a - c, b - d
    let manhattan_norm (a, b) = Int.abs a + Int.abs b
  end

  module Report = struct
    type t =
      { sensor : Pos.t
      ; beacon : Pos.t
      }
    [@@deriving sexp_of, compare]
  end

  module Input = struct
    type t = Report.t list

    let load in_channel =
      In_channel.input_lines in_channel
      |> List.map ~f:(fun s ->
             Scanf.sscanf
               s
               "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d"
               (fun sx sy bx by -> { Report.sensor = sx, sy; beacon = bx, by }))
    ;;
  end

  let number_of_beacons_on_line reports ~line =
    List.filter_map reports ~f:(fun (report : Report.t) ->
        if snd report.beacon = line then Some report.beacon else None)
    |> Pos.Set.of_list
    |> Set.length
  ;;

  let range_covered_by_sensor (report : Report.t) ~line =
    let x, _ = report.sensor in
    let projection = x, line in
    let distance = Pos.(manhattan_norm (projection - report.sensor)) in
    let beacon_distance = Pos.(manhattan_norm (report.beacon - report.sensor)) in
    let slack = beacon_distance - distance in
    if slack < 0 then None else Some (x - slack, x + slack)
  ;;

  let ranges_covered_by_sensors_rev reports ~line =
    List.filter_map reports ~f:(range_covered_by_sensor ~line)
    |> List.sort ~compare:Pos.compare
    |> List.fold ~init:[] ~f:(fun ranges ((new_lo, new_hi) as new_) ->
           assert (new_lo <= new_hi);
           match ranges with
           | [] -> [ new_ ]
           | (old_lo, old_hi) :: rest ->
             assert (old_lo <= new_lo);
             if old_hi >= new_lo
             then (old_lo, Int.max new_hi old_hi) :: rest
             else new_ :: ranges)
  ;;

  let count_clear reports ~line =
    let beacon_count = number_of_beacons_on_line reports ~line in
    let total_ranges =
      ranges_covered_by_sensors_rev reports ~line
      |> List.sum (module Int) ~f:(fun (lo, hi) -> hi - lo + 1)
    in
    total_ranges - beacon_count
  ;;

  let beacon_line_scan reports ~lo_limit ~hi_limit =
    let line_scan =
      let%bind.Sequence line =
        Sequence.range ~start:`inclusive ~stop:`inclusive lo_limit hi_limit
      in
      let ranges = ranges_covered_by_sensors_rev reports ~line |> List.rev in
      match
        List.fold_until
          ranges
          ~init:(lo_limit, hi_limit)
          ~f:(fun (lo_limit, hi_limit) (lo, hi) ->
            if lo_limit < lo
            then Stop (Some lo_limit)
            else if hi >= hi_limit
            then Stop None
            else Continue (hi + 1, hi_limit))
          ~finish:(fun (lo_limit, _) -> Some lo_limit)
      with
      | None -> Sequence.empty
      | Some x -> Sequence.return (x, line)
    in
    match Sequence.to_list line_scan with
    | [ pos ] -> pos
    | positions -> raise_s [%message "Many positions found" (positions : Pos.t list)]
  ;;

  module Part_1 = struct
    include Int_result

    let run = count_clear ~line:2000000
  end

  module Part_2 = struct
    include Int_result

    let run input =
      (* The small input has a smaller limit *)
      let hi_limit =
        match Sys.getenv "HI_LIMIT" with
        | None -> 4000000
        | Some limit -> Int.of_string limit
      in
      let x, y = beacon_line_scan input ~lo_limit:0 ~hi_limit in
      (x * 4000000) + y
    ;;
  end
end

let () = Framework.register ~day:15 (module Day_15)

module Day_16 = struct
  module Valve = struct
    type t =
      { name : string
      ; flow_rate : int
      ; connected_valves : string list
      }
  end

  module Input = struct
    type t = Valve.t list

    module Parse = struct
      open! Angstrom

      let number = take_while1 Char.is_digit |> map ~f:Int.of_string
      let name = take_while1 Char.is_uppercase

      let line =
        let%mapn.Angstrom name = string "Valve " *> name
        and flow_rate = string " has flow rate=" *> number
        and connected_valves =
          (string "; tunnels lead to valves " <|> string "; tunnel leads to valve ")
          *> sep_by1 (string ", ") name
        in
        { Valve.name; flow_rate; connected_valves }
      ;;

      let parser = many (line <* end_of_line)
      let parse s = Angstrom.parse_string ~consume:All parser s |> Result.ok_or_failwith
    end

    let load in_channel = In_channel.input_all in_channel |> Parse.parse
  end

  module Bitset = struct
    module Tracker = struct
      module Id = Unique_id.Int ()

      type t =
        { id : Id.t
        ; mapping : int String.Table.t
        ; mutable lowest_available : int
        }

      let create () =
        { id = Id.create (); mapping = String.Table.create (); lowest_available = 0 }
      ;;

      let lookup t v = Hashtbl.find t.mapping v

      let lookup_or_assign t v =
        match Hashtbl.find t.mapping v with
        | None ->
          let available = t.lowest_available in
          if available > 62
          then raise_s [%message "Can only support 63 elements in the bitset"];
          t.lowest_available <- available + 1;
          let bitset = 1 lsl available in
          Hashtbl.set t.mapping ~key:v ~data:bitset;
          bitset
        | Some n -> n
      ;;

      let compare a b = Id.compare a.id b.id
    end

    type t =
      { tracker : (Tracker.t[@sexp.opaque])
      ; set : int
      }
    [@@deriving sexp_of, compare]

    let empty tracker = { tracker; set = 0 }

    let mem t v =
      match Tracker.lookup t.tracker v with
      | None -> false
      | Some v -> Int.O.(t.set land v <> 0)
    ;;

    let add t v = { t with set = Tracker.lookup_or_assign t.tracker v lor t.set }

    let powerset_int =
      Memo.recursive ~hashable:Int.hashable (fun powerset_int set ->
          if Int.equal set 0
          then Int.Set.singleton 0
          else (
            let element = 1 lsl Int.ctz set in
            let subset_powerset = powerset_int (set lxor element) in
            Core.Set.union
              subset_powerset
              (Int.Set.map subset_powerset ~f:(fun set -> set lor element))))
    ;;

    let length t = Int.popcount t.set

    let powerset t =
      powerset_int t.set
      |> Core.Set.to_sequence
      |> Sequence.map ~f:(fun set -> { t with set })
    ;;

    let diff a b =
      assert (phys_equal a.tracker b.tracker);
      { a with set = a.set land lnot b.set }
    ;;

    let%expect_test _ =
      print_s [%sexp (powerset_int 5 : Int.Set.t)];
      [%expect {| (0 1 4 5) |}]
    ;;
  end

  let valve_map valves =
    List.map valves ~f:(fun valve -> valve.Valve.name, valve) |> String.Map.of_alist_exn
  ;;

  let distances start valves =
    let valves_of_interest =
      start
      :: List.filter_map valves ~f:(fun valve ->
             if valve.Valve.flow_rate > 0 then Some valve.name else None)
    in
    let valves_of_interest_set = String.Set.of_list valves_of_interest in
    let valve_map = valve_map valves in
    let bfs start =
      let queue = Queue.create () in
      let explored = String.Map.singleton start 0 in
      Queue.enqueue queue (start, 0);
      let rec loop explored =
        match Queue.dequeue queue with
        | None ->
          Map.filter_keys explored ~f:(fun key ->
              (not (String.equal key start)) && Set.mem valves_of_interest_set key)
        | Some (valve, cost) ->
          let cost = cost + 1 in
          (Map.find_exn valve_map valve).connected_valves
          |> List.fold ~init:explored ~f:(fun explored valve ->
                 if Map.mem explored valve
                 then explored
                 else (
                   Queue.enqueue queue (valve, cost);
                   Map.set explored ~key:valve ~data:cost))
          |> loop
      in
      loop explored
    in
    List.fold ~init:String.Map.empty valves_of_interest ~f:(fun adj valve ->
        Map.set adj ~key:valve ~data:((Map.find_exn valve_map valve).flow_rate, bfs valve))
  ;;

  module State = struct
    type t =
      { opened : Bitset.t
      ; total_flow : int
      ; total_open : int
      ; remaining_minutes : int
      ; valve : string
      }
    [@@deriving sexp_of]
  end

  let search ~can_open ~remaining_minutes ~distances ~start =
    let rec loop to_explore ~current_maximum =
      match to_explore with
      | [] -> current_maximum
      | { State.opened; total_flow; total_open; remaining_minutes; valve } :: to_explore
        ->
        if remaining_minutes <= 1 (* Seems to speed things up a bit *)
        then loop to_explore ~current_maximum
        else (
          let pressure, neighbors = Map.find_exn distances valve in
          let total_flow, total_open, remaining_minutes, opened =
            if Bitset.mem can_open valve
            then (
              let total_flow = total_flow + total_open in
              let total_open = total_open + pressure in
              let remaining_minutes = remaining_minutes - 1 in
              let opened = Bitset.add opened valve in
              total_flow, total_open, remaining_minutes, opened)
            else total_flow, total_open, remaining_minutes, opened
          in
          let current_maximum =
            Int.max current_maximum (total_flow + (total_open * remaining_minutes))
          in
          let to_explore =
            Map.fold
              neighbors
              ~init:to_explore
              ~f:(fun ~key:neighbor ~data:distance to_explore ->
                if distance <= remaining_minutes
                   && (not (Bitset.mem opened neighbor))
                   && Bitset.mem can_open neighbor
                then
                  { State.remaining_minutes = remaining_minutes - distance
                  ; total_open
                  ; total_flow = total_flow + (total_open * distance)
                  ; opened
                  ; valve = neighbor
                  }
                  :: to_explore
                else to_explore)
          in
          loop to_explore ~current_maximum)
    in
    loop
      [ { State.remaining_minutes
        ; total_open = 0
        ; valve = start
        ; total_flow = 0
        ; opened = Bitset.empty can_open.tracker
        }
      ]
      ~current_maximum:0
  ;;

  let all_useful_valves tracker valves =
    List.filter_map valves ~f:(fun valve ->
        if valve.Valve.flow_rate > 0 then Some valve.name else None)
    |> List.fold ~init:(Bitset.empty tracker) ~f:Bitset.add
  ;;

  let part_1 start valves ~remaining_minutes =
    let distances = distances start valves in
    let can_open = all_useful_valves (Bitset.Tracker.create ()) valves in
    search ~can_open ~remaining_minutes ~distances ~start
  ;;

  let split_set all_useful =
    all_useful
    |> Bitset.powerset
    |> Sequence.filter_map ~f:(fun set ->
           let diff = Bitset.diff all_useful set in
           if Bitset.compare set diff <= 0 then Some (set, diff) else None)
    |> Sequence.to_list
  ;;

  let part_2 valves ~remaining_minutes start =
    let tracker = Bitset.Tracker.create () in
    let all_useful = all_useful_valves tracker valves in
    let distances = distances start valves in
    let search can_open = search ~distances ~can_open ~remaining_minutes ~start in
    let set_split = split_set all_useful in
    set_split
    |> List.map ~f:(fun (human, elephant) -> search human + search elephant)
    |> List.max_elt ~compare
    |> Option.value ~default:0
  ;;

  module Part_1 = struct
    include Int_result

    let run input = part_1 "AA" input ~remaining_minutes:30
  end

  module Part_2 = struct
    include Int_result

    let run input = part_2 input "AA" ~remaining_minutes:26
  end
end

let () = Framework.register ~day:16 (module Day_16)

module Day_17 = struct
  module Direction = struct
    type t =
      | Left
      | Right
  end

  module Input = struct
    type t = Direction.t list

    let load in_channel =
      In_channel.input_all in_channel
      |> String.to_list
      |> List.filter_map ~f:(function
             | '<' -> Some Direction.Left
             | '>' -> Some Right
             | '\n' -> None
             | c -> raise_s [%message "Bad char" (c : char)])
    ;;
  end

  module Shapes = struct
    let horizontal = [ 0b11110 ]
    let plus = [ 0b01000; 0b11100; 0b01000 ]
    let flipped_l = [ 0b00100; 0b00100; 0b11100 ]
    let vertical = List.init 4 ~f:(const 0b10000)
    let square = List.init 2 ~f:(const 0b11000)
    let all = [ horizontal; plus; flipped_l; vertical; square ]

    let line_to_string line =
      String.init 7 ~f:(fun i -> if (1 lsl (6 - i)) land line <> 0 then '#' else '.')
    ;;

    let shape_to_string shape =
      List.map shape ~f:line_to_string |> String.concat ~sep:"\n"
    ;;

    let%expect_test _ =
      List.map all ~f:shape_to_string |> String.concat ~sep:"\n\n" |> print_endline;
      [%expect
        {|
        ..####.

        ...#...
        ..###..
        ...#...

        ....#..
        ....#..
        ..###..

        ..#....
        ..#....
        ..#....
        ..#....

        ..##...
        ..##... |}]
    ;;
  end

  let full_line = 0b1_111_111

  let validate_top map ~top ~shape =
    List.for_alli shape ~f:(fun i line ->
        i <= top
        && Map.find map (top - i) |> Option.value ~default:0 |> ( land ) line |> ( = ) 0)
  ;;

  let validate_shape map ~top ~shape ~original_shape =
    List.for_all2_exn shape original_shape ~f:(fun a b -> Int.popcount a = Int.popcount b)
    && validate_top map ~top ~shape
  ;;

  let shift shape ~f = List.map shape ~f:(fun line -> f line land full_line)

  let shift shape direction =
    shift
      shape
      ~f:
        (match direction with
        | Direction.Left -> fun line -> line lsl 1
        | Right -> fun line -> line lsr 1)
  ;;

  let%expect_test _ =
    shift Shapes.flipped_l Right |> Shapes.shape_to_string |> print_endline;
    [%expect {|
      .....#.
      .....#.
      ...###. |}]
  ;;

  let simulate_single_drop ~top map direction shape =
    let shifted = shift shape direction in
    let shape =
      if validate_shape map ~top ~shape:shifted ~original_shape:shape
      then shifted
      else shape
    in
    if validate_top map ~top:(top - 1) ~shape
    then `Dropped shape
    else
      `Settled
        (List.foldi shape ~init:map ~f:(fun i map line ->
             Map.update map (top - i) ~f:(function
                 | None -> line
                 | Some existing -> existing lor line)))
  ;;

  let rec simulate_rock ~top map rock wind_sequence =
    let (index, direction), wind_sequence =
      Sequence.next wind_sequence |> Option.value_exn
    in
    match simulate_single_drop ~top map direction rock with
    | `Dropped shape -> simulate_rock ~top:(top - 1) map shape wind_sequence
    | `Settled map -> index, map, top + 1, wind_sequence
  ;;

  let simulate_rock ~top map rock wind_sequence =
    simulate_rock ~top:(top + List.length rock + 2) map rock wind_sequence
  ;;

  let rec shift_right ~f line mask =
    if f (line land mask) 0
    then (
      let new_mask = mask lsr 1 in
      if new_mask = 0 then mask else shift_right ~f line new_mask)
    else mask lsl 1
  ;;

  let rec shift_left ~f line mask =
    if f (line land mask) 0
    then (
      let new_mask = mask lsl 1 in
      if new_mask > full_line then mask else shift_left ~f line new_mask)
    else mask lsr 1
  ;;

  let is_set_f = ( <> )
  let unset_f = ( = )

  let left_hole line mask =
    if line land mask <> 0
    then shift_right ~f:is_set_f line mask
    else shift_left ~f:unset_f line mask
  ;;

  let right_hole line mask =
    if line land mask <> 0
    then shift_left ~f:is_set_f line mask
    else shift_right ~f:unset_f line mask
  ;;

  let visibility map =
    Map.to_sequence ~order:`Decreasing_key map
    |> Sequence.fold_until
         ~init:(0b1_000_00, 0b1, [])
         ~f:(fun (left_mask, right_mask, visibility) (_, line) ->
           let left_mask = left_hole line left_mask in
           let right_mask = right_hole line right_mask in
           if right_mask >= left_mask
           then Stop visibility
           else Continue (left_mask, right_mask, line :: visibility))
         ~finish:(fun (_, _, visibility) -> visibility)
  ;;

  module Repetition_tracker = struct
    module T = struct
      type t =
        { rock_index : int
        ; wind_index : int
        ; visibility : int list
        }
      [@@deriving sexp_of, compare]
    end

    include T
    include Comparable.Make_plain (T)
  end

  let compute_cycle wind_sequence rock_sequence =
    Sequence.fold_until
      rock_sequence
      ~init:(Int.Map.empty, wind_sequence, 0, 0, Repetition_tracker.Map.empty, 0)
      ~f:
        (fun (map, wind_sequence, top, wind_index, repetition_tracker, i)
             (rock_index, rock) ->
        let visibility = visibility map in
        let key = { Repetition_tracker.rock_index; wind_index; visibility } in
        match Map.find repetition_tracker key with
        | Some (previous_index, previous_top) ->
          Stop (previous_index, i - previous_index, top - previous_top)
        | None ->
          let repetition_tracker = Map.set repetition_tracker ~key ~data:(i, top) in
          let wind_index, map, new_top, wind_sequence =
            simulate_rock ~top map rock wind_sequence
          in
          Continue
            ( map
            , wind_sequence
            , Int.max top new_top
            , wind_index
            , repetition_tracker
            , i + 1 ))
      ~finish:(fun _ -> assert false)
  ;;

  let simulate input ~count =
    let cycle_with_indices list =
      List.mapi list ~f:Tuple2.create |> Sequence.cycle_list_exn
    in
    let wind_sequence = cycle_with_indices input in
    let rock_sequence = cycle_with_indices Shapes.all in
    let repeat_start, repeat_cycle, height = compute_cycle wind_sequence rock_sequence in
    let _, _, top =
      Sequence.fold
        (Sequence.take
           rock_sequence
           (((count - repeat_start) % repeat_cycle) + repeat_start))
        ~init:(Int.Map.empty, wind_sequence, 0)
        ~f:(fun (map, wind_sequence, top) (_rock_index, rock) ->
          let _wind_index, map, new_top, wind_sequence =
            simulate_rock ~top map rock wind_sequence
          in
          map, wind_sequence, Int.max top new_top)
    in
    (Int.max 0 (count - repeat_start) / repeat_cycle * height) + top
  ;;

  module Part_1 = struct
    include Int_result

    let run input = simulate input ~count:2022
  end

  module Part_2 = struct
    include Int_result

    let run input = simulate input ~count:1_000_000_000_000
  end
end

let () = Framework.register ~day:17 (module Day_17)

module Day_18 = struct
  module Coord = struct
    module T = struct
      type t =
        { x : int
        ; y : int
        ; z : int
        }
      [@@deriving sexp_of, compare, hash, fields]
    end

    include T
    include Comparable.Make_plain (T)
    include Hashable.Make_plain (T)

    let neighbors { x; y; z } =
      List.map
        ~f:(fun (dx, dy, dz) -> { x = x + dx; y = y + dy; z = z + dz })
        [ -1, 0, 0; 1, 0, 0; 0, -1, 0; 0, 1, 0; 0, 0, -1; 0, 0, 1 ]
    ;;
  end

  module Input = struct
    type t = Coord.t list

    let load in_channel =
      In_channel.input_lines in_channel
      |> List.map ~f:(fun line ->
             Scanf.sscanf line "%d,%d,%d" (fun x y z -> { Coord.x; y; z }))
    ;;
  end

  let total_surface_area_set set =
    Set.sum
      (module Int)
      set
      ~f:(fun coord ->
        List.count (Coord.neighbors coord) ~f:(fun coord -> not (Set.mem set coord)))
  ;;

  let total_surface_area input =
    let set = Coord.Set.of_list input in
    total_surface_area_set set
  ;;

  let edges (input : Coord.t list) =
    let reduce ~f =
      List.reduce_exn input ~f:(fun a b ->
          { x = f a.x b.x; y = f a.y b.y; z = f a.z b.z })
    in
    reduce ~f:Int.min, reduce ~f:Int.max
  ;;

  let all_relevant ~(minimum : Coord.t) ~(maximum : Coord.t) =
    let sequence f = Sequence.range ~stop:`inclusive (f minimum) (f maximum) in
    let%bind.Sequence x = sequence Coord.x in
    let%bind.Sequence y = sequence Coord.y in
    let%map.Sequence z = sequence Coord.z in
    { Coord.x; y; z }
  ;;

  let out_of_edge ~(minimum : Coord.t) ~(maximum : Coord.t) { Coord.x; y; z } =
    x < minimum.x
    || y < minimum.y
    || z < minimum.z
    || x > maximum.x
    || y > maximum.y
    || z > maximum.z
  ;;

  let search_exposure ~minimum ~maximum exposure start =
    match Map.find exposure start with
    | Some v -> v, exposure
    | None ->
      let explored = Coord.Set.singleton start in
      let rec loop explored = function
        | [] -> `Not_exposed, explored
        | coord :: rest ->
          let neighbors =
            Coord.neighbors coord
            |> List.filter ~f:(fun neighbor ->
                   (not (Set.mem explored neighbor))
                   &&
                   match Map.find exposure neighbor with
                   | Some `Not_exposed -> false
                   | None | Some `Exposed -> true)
          in
          let explored = Coord.Set.of_list neighbors |> Set.union explored in
          if List.exists neighbors ~f:(fun neighbor ->
                 out_of_edge ~minimum ~maximum neighbor
                 ||
                 match Map.find exposure neighbor with
                 | Some `Exposed -> true
                 | None | Some `Not_exposed -> false)
          then `Exposed, explored
          else loop explored (List.rev_append neighbors rest)
      in
      let result, explored = loop explored [ start ] in
      ( result
      , Set.fold explored ~init:exposure ~f:(fun exposure coord ->
            Map.set exposure ~key:coord ~data:result) )
  ;;

  let bound input =
    let minimum, maximum = edges input in
    let set = Coord.Set.of_list input in
    let exposure = Coord.Map.of_key_set ~f:(const `Not_exposed) set in
    let _exposure, not_exposed =
      Sequence.fold
        (all_relevant ~minimum ~maximum)
        ~init:(exposure, Coord.Set.empty)
        ~f:(fun (exposure, not_exposed) coord ->
          let result, exposure = search_exposure ~minimum ~maximum exposure coord in
          ( exposure
          , match result with
            | `Exposed -> not_exposed
            | `Not_exposed -> Set.add not_exposed coord ))
    in
    total_surface_area_set not_exposed
  ;;

  module Part_1 = struct
    include Int_result

    let run = total_surface_area
  end

  module Part_2 = struct
    include Int_result

    let run = bound
  end
end

let () = Framework.register ~day:18 (module Day_18)

module Day_19 = struct
  module Cost = struct
    type t =
      { ore : int
      ; clay : int
      ; obsidian : int
      }
    [@@deriving sexp_of, compare, hash, fields]

    let create ?(ore = 0) ?(clay = 0) ?(obsidian = 0) () = { ore; clay; obsidian }
    let zero = create ()

    module O = struct
      let ( >=$ ) a b = a.ore >= b.ore && a.clay >= b.clay && a.obsidian >= b.obsidian

      let ( -$ ) a b =
        { ore = a.ore - b.ore
        ; clay = a.clay - b.clay
        ; obsidian = a.obsidian - b.obsidian
        }
      ;;

      let ( +$ ) a b =
        { ore = a.ore + b.ore
        ; clay = a.clay + b.clay
        ; obsidian = a.obsidian + b.obsidian
        }
      ;;
    end

    include O
  end

  module Blueprint = struct
    type t =
      { index : int
      ; ore_robot : Cost.t
      ; clay_robot : Cost.t
      ; obsidian_robot : Cost.t
      ; geode_robot : Cost.t
      }
    [@@deriving sexp_of, compare, hash]
  end

  module Input = struct
    type t = Blueprint.t list

    let load in_channel =
      In_channel.input_lines in_channel
      |> List.map ~f:(fun s ->
             Scanf.sscanf
               s
               "Blueprint %d: Each ore robot costs %d ore. Each clay robot costs %d ore. \
                Each obsidian robot costs %d ore and %d clay. Each geode robot costs %d \
                ore and %d obsidian."
               (fun
                 index
                 ore_robot_ore
                 clay_robot_ore
                 obsidian_robot_ore
                 obsidian_robot_clay
                 geode_robot_ore
                 geode_robot_obsidian
               ->
                 { Blueprint.index
                 ; ore_robot = Cost.create ~ore:ore_robot_ore ()
                 ; clay_robot = Cost.create ~ore:clay_robot_ore ()
                 ; obsidian_robot =
                     Cost.create ~ore:obsidian_robot_ore ~clay:obsidian_robot_clay ()
                 ; geode_robot =
                     Cost.create ~ore:geode_robot_ore ~obsidian:geode_robot_obsidian ()
                 }))
    ;;
  end

  module State = struct
    module T = struct
      type t =
        { inventory : Cost.t
        ; ore_robots : int
        ; clay_robots : int
        ; obsidian_robots : int
        ; geode_robots : int
        ; minutes_remaining : int
        ; geode_count : int
        }
      [@@deriving sexp_of, compare, hash, fields]
    end

    include T
    include Comparable.Make_plain (T)
    include Hashable.Make_plain (T)
  end

  let compute (blueprint : Blueprint.t) =
    let current_max = ref 0 in
    let max_compute ~f =
      List.map
        ~f
        [ blueprint.ore_robot
        ; blueprint.geode_robot
        ; blueprint.clay_robot
        ; blueprint.obsidian_robot
        ]
      |> List.max_elt ~compare
      |> Option.value_exn
    in
    let max_ore = max_compute ~f:Cost.ore in
    let max_clay = max_compute ~f:Cost.clay in
    let max_obsidian = max_compute ~f:Cost.obsidian in
    Memo.recursive ~hashable:State.hashable (fun compute state ->
        let open Cost.O in
        if state.minutes_remaining <= 0
        then (
          current_max := Int.max state.geode_count !current_max;
          state.geode_count)
        else if (state.minutes_remaining * state.geode_robots)
                + (state.minutes_remaining * (state.minutes_remaining - 1) / 2)
                + state.geode_count
                <= !current_max
        then state.geode_count (* Will get disregarded *)
        else (
          let collectable_this_round =
            Cost.create
              ~ore:state.ore_robots
              ~clay:state.clay_robots
              ~obsidian:state.obsidian_robots
              ()
          in
          let geode_count = state.geode_count + state.geode_robots in
          let maybe_buy_robot field (cost : Cost.t) =
            if state.inventory >=$ cost
            then
              Some
                (Fieldslib.Field.fset
                   field
                   { state with inventory = state.inventory -$ cost }
                   (Fieldslib.Field.get field state + 1))
            else None
          in
          let new_states =
            state
            :: List.filter_opt
                 [ maybe_buy_robot State.Fields.ore_robots blueprint.ore_robot
                 ; maybe_buy_robot State.Fields.clay_robots blueprint.clay_robot
                 ; maybe_buy_robot State.Fields.obsidian_robots blueprint.obsidian_robot
                 ; maybe_buy_robot State.Fields.geode_robots blueprint.geode_robot
                 ]
          in
          List.map new_states ~f:(fun state ->
              (* No point in doing this. These conditions might seem contradictory, but if
               * the first option matches and we didn't consider building a geode robot,
               * then we're being inefficient. *)
              if (state.inventory >=$ blueprint.ore_robot
                 && state.inventory >=$ blueprint.geode_robot
                 && state.inventory >=$ blueprint.obsidian_robot
                 && state.inventory >=$ blueprint.clay_robot)
                 || state.ore_robots > max_ore
                 || state.obsidian_robots > max_obsidian
                 || state.clay_robots > max_clay
              then 0
              else (
                let state =
                  { state with
                    geode_count
                  ; minutes_remaining = state.minutes_remaining - 1
                  ; inventory = state.inventory +$ collectable_this_round
                  }
                in
                compute state))
          |> List.max_elt ~compare
          |> Option.value ~default:0))
  ;;

  let quality_level blueprint =
    let state =
      { State.inventory = Cost.zero
      ; ore_robots = 1
      ; clay_robots = 0
      ; obsidian_robots = 0
      ; geode_robots = 0
      ; minutes_remaining = 24
      ; geode_count = 0
      }
    in
    let result = compute blueprint state in
    Gc.compact ();
    result * blueprint.index
  ;;

  let total_quality blueprints = List.sum (module Int) blueprints ~f:quality_level

  let just_input blueprint =
    let state =
      { State.inventory = Cost.zero
      ; ore_robots = 1
      ; clay_robots = 0
      ; obsidian_robots = 0
      ; geode_robots = 0
      ; minutes_remaining = 32
      ; geode_count = 0
      }
    in
    let result = compute blueprint state in
    Gc.compact ();
    result
  ;;

  let product blueprints =
    List.fold ~init:1 (List.take blueprints 3) ~f:(fun product blueprint ->
        product * just_input blueprint)
  ;;

  module Part_1 = struct
    include Int_result

    let run = total_quality
  end

  module Part_2 = struct
    include Int_result

    let run = product
  end
end

let () = Framework.register ~day:19 (module Day_19)

module Day_20 = struct
  module Input = struct
    type t = int list

    let load in_channel = In_channel.input_lines in_channel |> List.map ~f:Int.of_string
  end

  module Chain = struct
    type t =
      { value : int
      ; mutable prev : t option
      ; mutable next : t option
      }

    let prev t = Option.value_exn t.prev
    let next t = Option.value_exn t.next
    let set_prev t prev = t.prev <- Some prev
    let set_next t next = t.next <- Some next
  end

  let rec make_chain (head : Chain.t) (prev : Chain.t) acc = function
    | [] ->
      Chain.set_next prev head;
      Chain.set_prev head prev;
      prev.next <- Some head;
      head.prev <- Some prev;
      head :: List.rev acc
    | value :: rest ->
      let node = { Chain.value; prev = Some prev; next = None } in
      Chain.set_next prev node;
      make_chain head node (node :: acc) rest
  ;;

  let make_chain = function
    | [] -> raise_s [%message "Cannot make chain from an empty list"]
    | value :: rest ->
      let head = { Chain.value; prev = None; next = None } in
      make_chain head head [] rest
  ;;

  let rec locate node n =
    if n = 0
    then node
    else if n < 0
    then locate (Chain.prev node) (n + 1)
    else locate (Chain.next node) (n - 1)
  ;;

  let move node n =
    let old_next = Chain.next node in
    let old_prev = Chain.prev node in
    Chain.set_next old_prev old_next;
    Chain.set_prev old_next old_prev;
    let new_next = locate old_next n in
    let new_prev = Chain.prev new_next in
    Chain.set_next node new_next;
    Chain.set_prev node new_prev;
    Chain.set_prev new_next node;
    Chain.set_next new_prev node
  ;;

  let solve input ~decryption_key ~number_of_mixes =
    let input = List.map input ~f:(( * ) decryption_key) in
    let length = List.length input in
    let chain = make_chain input in
    for _ = 1 to number_of_mixes do
      List.iter chain ~f:(fun node -> move node (node.value mod (length - 1)))
    done;
    let zero_node = List.find_exn chain ~f:(fun chain -> chain.value = 0) in
    (locate zero_node 1000).value
    + (locate zero_node 2000).value
    + (locate zero_node 3000).value
  ;;

  module Part_1 = struct
    include Int_result

    let run input = solve input ~decryption_key:1 ~number_of_mixes:1
  end

  module Part_2 = struct
    include Int_result

    let run input = solve input ~decryption_key:811589153 ~number_of_mixes:10
  end
end

let () = Framework.register ~day:20 (module Day_20)

module Day_21 = struct
  module Decimal = struct
    type t =
      { num : int
      ; den : int
      }
    [@@deriving compare, equal]

    let of_int num = { num; den = 1 }
    let zero = of_int 0
    let rec gcd a b = if b = 0 then a else gcd b (a mod b)

    let to_string { num; den } =
      if den = 1 then Int.to_string num else sprintf "%d/%d" num den
    ;;

    let simplify { num; den } =
      if den = 0
      then raise_s [%message "Denominator is 0"]
      else if num = 0
      then zero
      else (
        let gcd = gcd num den in
        let t = { num = num / gcd; den = den / gcd } in
        if t.den < 0 then { num = -t.num; den = -t.den } else t)
    ;;

    let ( + ) a b =
      simplify { num = (a.num * b.den) + (b.num * a.den); den = a.den * b.den }
    ;;

    let ( - ) a b =
      simplify { num = (a.num * b.den) - (b.num * a.den); den = a.den * b.den }
    ;;

    let ( / ) a b = simplify { num = a.num * b.den; den = a.den * b.num }
    let ( * ) a b = simplify { num = a.num * b.num; den = a.den * b.den }
  end

  module With_unknown = struct
    (* a0 + a1 * x *)
    type t =
      { a0 : Decimal.t
      ; a1 : Decimal.t
      }

    let to_string { a0; a1 } = sprintf !"%{Decimal} + %{Decimal}*x" a0 a1

    module O = struct
      let ( + ) x y = Decimal.{ a0 = x.a0 + y.a0; a1 = x.a1 + y.a1 }
      let ( - ) x y = Decimal.{ a0 = x.a0 - y.a0; a1 = x.a1 - y.a1 }

      let ( * ) x y =
        let mul { a0; a1 } d = Decimal.{ a0 = a0 * d; a1 = a1 * d } in
        if x.a1.num = 0
        then mul y x.a0
        else if y.a1.num = 0
        then mul x y.a0
        else
          raise_s
            [%message
              "Cannot multiply two numbers with an x component"
                (to_string x)
                (to_string y)]
      ;;

      let ( / ) x y =
        if y.a1.num = 0
        then Decimal.{ a0 = x.a0 / y.a0; a1 = x.a1 / y.a0 }
        else raise_s [%message "Cannot divide by x" (to_string x) (to_string y)]
      ;;
    end

    include O
  end

  module Operation = struct
    type t =
      | Add
      | Sub
      | Mul
      | Div
    [@@deriving sexp_of, compare]

    let operation =
      let open Decimal in
      function
      | Add -> ( + )
      | Sub -> ( - )
      | Mul -> ( * )
      | Div -> ( / )
    ;;

    let operation_with_unknown =
      let open With_unknown in
      function
      | Add -> ( + )
      | Sub -> ( - )
      | Mul -> ( * )
      | Div -> ( / )
    ;;
  end

  module Expr = struct
    type t =
      | Value of int
      | Operation of Operation.t * string * string
    [@@deriving sexp_of, compare]
  end

  module Monkey = struct
    type t =
      { name : string
      ; expr : Expr.t
      }
    [@@deriving sexp_of, compare]
  end

  module Input = struct
    type t = Monkey.t list

    module Parse = struct
      open! Angstrom

      let number = take_while1 Char.is_digit |> map ~f:Int.of_string
      let monkey = take_while1 Char.is_alpha

      let operation =
        let open Operation in
        choice
          [ char '+' *> return Add
          ; char '-' *> return Sub
          ; char '*' *> return Mul
          ; char '/' *> return Div
          ]
      ;;

      let expr =
        lift (fun number -> Expr.Value number) number
        <|> let%mapn.Angstrom a = monkey
            and operation = char ' ' *> operation <* char ' '
            and b = monkey in
            Expr.Operation (operation, a, b)
      ;;

      let line =
        let%mapn.Angstrom name = monkey <* string ": "
        and expr = expr in
        { Monkey.name; expr }
      ;;

      let parser = many (line <* end_of_line)
      let parse s = Angstrom.parse_string ~consume:All parser s |> Result.ok_or_failwith
    end

    let load in_channel = In_channel.input_all in_channel |> Parse.parse
  end

  let solve input =
    let monkey_map =
      List.map input ~f:(fun { Monkey.name; expr } -> name, expr)
      |> String.Map.of_alist_exn
    in
    let solve =
      Memo.recursive ~hashable:String.hashable (fun solve monkey ->
          match Map.find_exn monkey_map monkey with
          | Value i -> Decimal.of_int i
          | Operation (operation, a, b) ->
            Operation.operation operation (solve a) (solve b))
    in
    solve "root"
  ;;

  (* Used this to determine that we can solve as a system of linear
   * equations. *)
  let solve_with_unknown input =
    let monkey_map =
      List.map input ~f:(fun { Monkey.name; expr } -> name, expr)
      |> String.Map.of_alist_exn
    in
    let solve =
      Memo.recursive ~hashable:String.hashable (fun solve monkey ->
          if String.equal monkey "humn"
          then { With_unknown.a0 = Decimal.zero; a1 = Decimal.of_int 1 }
          else (
            match Map.find_exn monkey_map monkey with
            | Value value -> { With_unknown.a0 = Decimal.of_int value; a1 = Decimal.zero }
            | Operation (operation, a, b) ->
              Operation.operation_with_unknown operation (solve a) (solve b)))
    in
    match Map.find_exn monkey_map "root" with
    | Operation (_, a, b) ->
      let zero_equation = With_unknown.( - ) (solve a) (solve b) in
      Decimal.(zero - (zero_equation.a0 / zero_equation.a1))
    | Value n -> raise_s [%message "Root is constant" (n : int)]
  ;;

  module Part_1 = struct
    type t = Decimal.t

    let show decimal = Decimal.to_string decimal |> print_endline
    let run = solve
  end

  module Part_2 = struct
    type t = Decimal.t

    let show decimal = Decimal.to_string decimal |> print_endline
    let run = solve_with_unknown
  end
end

let () = Framework.register ~day:21 (module Day_21)

module Day_22 = struct
  module Rotation = struct
    type t =
      | Right
      | Left
    [@@deriving sexp_of, compare]
  end

  module Direction = struct
    type t =
      | Right
      | Down
      | Left
      | Up
    [@@deriving sexp_of, compare]

    let rotate t rotation =
      match t, (rotation : Rotation.t) with
      | Right, Right -> Down
      | Right, Left -> Up
      | Down, Right -> Left
      | Down, Left -> Right
      | Left, Right -> Up
      | Left, Left -> Down
      | Up, Right -> Right
      | Up, Left -> Left
    ;;

    let score = function
      | Right -> 0
      | Down -> 1
      | Left -> 2
      | Up -> 3
    ;;
  end

  module Action = struct
    type t =
      | Step of int
      | Rotate of Rotation.t
    [@@deriving sexp_of, compare, variants]
  end

  module Input = struct
    type t =
      { grid : [ `Wall | `Open ] option array array
      ; actions : Action.t list
      }
    [@@deriving sexp_of, compare]

    module Parse = struct
      open! Angstrom

      let grid_element =
        choice
          [ char ' ' *> return None
          ; char '.' *> return (Some `Open)
          ; char '#' *> return (Some `Wall)
          ]
      ;;

      let grid_line = many1 grid_element |> lift Array.of_list
      let grid = many1 (grid_line <* end_of_line) |> lift Array.of_list
      let number = take_while1 Char.is_digit |> map ~f:Int.of_string

      let direction =
        choice Rotation.[ char 'R' *> return Right; char 'L' *> return Left ]
      ;;

      let action = choice [ lift Action.step number; lift Action.rotate direction ]
      let actions = many action

      let parser =
        let%mapn.Angstrom grid = grid <* end_of_line
        and actions = actions <* end_of_line in
        { grid; actions }
      ;;

      let parse s = Angstrom.parse_string ~consume:All parser s |> Result.ok_or_failwith
    end

    let load in_channel = In_channel.input_all in_channel |> Parse.parse
  end

  module Pos = struct
    module T = struct
      type t =
        { row : int
        ; column : int
        }
      [@@deriving sexp_of, compare]
    end

    include T
    include Comparable.Make_plain (T)

    let of_indices ~row ~column = { row; column }
  end

  module Vec3 = struct
    module T = struct
      type t =
        { x : int
        ; y : int
        ; z : int
        }
      [@@deriving sexp_of, compare]
    end

    include T
    include Comparable.Make_plain (T)

    let zero = { x = 0; y = 0; z = 0 }

    let cross a b =
      { x = (a.y * b.z) - (a.z * b.y)
      ; y = (a.z * b.x) - (a.x * b.z)
      ; z = (a.x * b.y) - (a.y * b.x)
      }
    ;;

    let parallel a b = equal (cross a b) zero
  end

  module Cube_face = struct
    type t =
      { up : Vec3.t
      ; right : Vec3.t
      }
    [@@deriving sexp_of, compare]

    let normal t = Vec3.cross t.right t.up

    let rotate t direction =
      match (direction : Direction.t) with
      | Right -> { t with right = Vec3.cross t.up t.right }
      | Left -> { t with right = Vec3.cross t.right t.up }
      | Up -> { t with up = Vec3.cross t.up t.right }
      | Down -> { t with up = Vec3.cross t.right t.up }
    ;;

    let%expect_test _ =
      (* Based on the cube map in wikipedia *)
      let start = { up = { x = 0; y = 1; z = 0 }; right = { x = 1; y = 0; z = 0 } } in
      print_s [%message (normal start : Vec3.t)];
      [%expect {| ("normal start" ((x 0) (y 0) (z 1))) |}];
      print_s [%message (rotate start Left : t)];
      [%expect
        {| ("rotate start Left" ((up ((x 0) (y 1) (z 0))) (right ((x 0) (y 0) (z 1))))) |}];
      print_s [%message (rotate start Up : t)];
      [%expect
        {| ("rotate start Up" ((up ((x 0) (y 0) (z -1))) (right ((x 1) (y 0) (z 0))))) |}];
      print_s [%message (rotate start Down : t)];
      [%expect
        {| ("rotate start Down" ((up ((x 0) (y 0) (z 1))) (right ((x 1) (y 0) (z 0))))) |}];
      print_s [%message (rotate start Right : t)];
      [%expect
        {|
          ("rotate start Right"
           ((up ((x 0) (y 1) (z 0))) (right ((x 0) (y 0) (z -1))))) |}];
      print_s [%message (rotate (rotate start Right) Right : t)];
      [%expect
        {|
          ("rotate (rotate start Right) Right"
           ((up ((x 0) (y 1) (z 0))) (right ((x -1) (y 0) (z 0))))) |}]
    ;;
  end

  (* Normalized face coordinates are such that each square is 1x1 wide.
   * We want to return a mapping from position to the normal vector which
   * uniquely identifies a face (since we're always folding on the outside)
   * and a map from the normal vector to the coordinates (Cube_face.t) which
   * will indicate the orientation in which the input maps to the cube face
   * and the normalized face coordinate which will allow us to locate the next
   * step post rotation.
   *)
  let fold normalized_face_coordinates =
    let start_face =
      { Cube_face.up = { x = 0; y = 1; z = 0 }; right = { x = 1; y = 0; z = 0 } }
    in
    let neighbors (pos : Pos.t) =
      [ { pos with column = pos.column + 1 }, Direction.Right
      ; { pos with column = pos.column - 1 }, Left
      ; { pos with row = pos.row - 1 }, Up
      ; { pos with row = pos.row + 1 }, Down
      ]
      |> List.filter ~f:(fun (pos, _direction) -> Set.mem normalized_face_coordinates pos)
    in
    let start = Set.min_elt_exn normalized_face_coordinates in
    let explored = Pos.Set.singleton start in
    let rec loop explored pos_map normal_map = function
      | [] -> pos_map, normal_map
      | (position, face) :: to_explore ->
        let normal = Cube_face.normal face in
        let pos_map = Map.add_exn pos_map ~key:position ~data:normal in
        let normal_map = Map.add_exn normal_map ~key:normal ~data:(face, position) in
        let explored, to_explore =
          neighbors position
          |> List.fold
               ~init:(explored, to_explore)
               ~f:(fun (explored, to_explore) (pos, direction) ->
                 if Set.mem explored pos
                 then explored, to_explore
                 else (
                   let explored = Set.add explored pos in
                   let face = Cube_face.rotate face direction in
                   explored, (pos, face) :: to_explore))
        in
        loop explored pos_map normal_map to_explore
    in
    loop explored Pos.Map.empty Vec3.Map.empty [ start, start_face ]
  ;;

  let%expect_test _ =
    let input_faces =
      [ 0, 2; 1, 0; 1, 1; 1, 2; 2, 2; 2, 3 ]
      |> List.map ~f:(fun (row, column) -> Pos.of_indices ~row ~column)
      |> Pos.Set.of_list
    in
    let position_map, normal_map = fold input_faces in
    print_s
      [%message
        (position_map : Vec3.t Pos.Map.t) (normal_map : (Cube_face.t * Pos.t) Vec3.Map.t)];
    [%expect
      {|
      ((position_map
        ((((row 0) (column 2)) ((x 0) (y 0) (z 1)))
         (((row 1) (column 0)) ((x 0) (y 1) (z 0)))
         (((row 1) (column 1)) ((x -1) (y 0) (z 0)))
         (((row 1) (column 2)) ((x 0) (y -1) (z 0)))
         (((row 2) (column 2)) ((x 0) (y 0) (z -1)))
         (((row 2) (column 3)) ((x 1) (y 0) (z 0)))))
       (normal_map
        ((((x -1) (y 0) (z 0))
          (((up ((x 0) (y 0) (z 1))) (right ((x 0) (y -1) (z 0))))
           ((row 1) (column 1))))
         (((x 0) (y -1) (z 0))
          (((up ((x 0) (y 0) (z 1))) (right ((x 1) (y 0) (z 0))))
           ((row 1) (column 2))))
         (((x 0) (y 0) (z -1))
          (((up ((x 0) (y -1) (z 0))) (right ((x 1) (y 0) (z 0))))
           ((row 2) (column 2))))
         (((x 0) (y 0) (z 1))
          (((up ((x 0) (y 1) (z 0))) (right ((x 1) (y 0) (z 0))))
           ((row 0) (column 2))))
         (((x 0) (y 1) (z 0))
          (((up ((x 0) (y 0) (z 1))) (right ((x -1) (y 0) (z 0))))
           ((row 1) (column 0))))
         (((x 1) (y 0) (z 0))
          (((up ((x 0) (y -1) (z 0))) (right ((x 0) (y 0) (z 1))))
           ((row 2) (column 3))))))) |}]
  ;;

  let coordinates_in_range { Pos.row; column } ~side_length =
    row >= 0 && row < side_length && column >= 0 && column < side_length
  ;;

  let reset_coordinates { Pos.row; column } ~side_length =
    { Pos.row = row % side_length; column = column % side_length }
  ;;

  (* Returns action for row coordinates and action for column coordinates. *)
  let coordinate_adjustment
      ~(original_coordinates : Cube_face.t)
      ~(desired_coordinates : Cube_face.t)
    =
    if Vec3.parallel original_coordinates.up desired_coordinates.up
    then (
      assert (Vec3.parallel original_coordinates.right desired_coordinates.right);
      ( (if Vec3.equal original_coordinates.up desired_coordinates.up
        then `Same, `Row
        else `Flip, `Row)
      , if Vec3.equal original_coordinates.right desired_coordinates.right
        then `Same, `Column
        else `Flip, `Column ))
    else (
      assert (Vec3.parallel original_coordinates.up desired_coordinates.right);
      assert (Vec3.parallel original_coordinates.right desired_coordinates.up);
      ( (if Vec3.equal original_coordinates.up desired_coordinates.right
        then `Same, `Column
        else `Flip, `Column)
      , if Vec3.equal original_coordinates.right desired_coordinates.up
        then `Same, `Row
        else `Flip, `Row ))
  ;;

  let%expect_test _ =
    let sexp_of_adjustment = [%sexp_of: [ `Same | `Flip ] * [ `Row | `Column ]] in
    let face1 =
      { Cube_face.up = { x = 0; y = 1; z = 0 }; right = { x = 1; y = 0; z = 0 } }
    in
    let face2 =
      { Cube_face.up = { x = 0; y = 1; z = 0 }; right = { x = -1; y = 0; z = 0 } }
    in
    let face3 =
      { Cube_face.up = { x = -1; y = 0; z = 0 }; right = { x = 0; y = 1; z = 0 } }
    in
    print_s
      [%message
        (coordinate_adjustment ~original_coordinates:face1 ~desired_coordinates:face1
          : adjustment * adjustment)];
    print_s
      [%message
        (coordinate_adjustment ~original_coordinates:face1 ~desired_coordinates:face2
          : adjustment * adjustment)];
    print_s
      [%message
        (coordinate_adjustment ~original_coordinates:face1 ~desired_coordinates:face3
          : adjustment * adjustment)];
    [%expect
      {|
      ("coordinate_adjustment ~original_coordinates:face1 ~desired_coordinates:face1"
       ((Same Row) (Same Column)))
      ("coordinate_adjustment ~original_coordinates:face1 ~desired_coordinates:face2"
       ((Same Row) (Flip Column)))
      ("coordinate_adjustment ~original_coordinates:face1 ~desired_coordinates:face3"
       ((Same Column) (Flip Row))) |}]
  ;;

  let adjust_position
      ~(original_coordinates : Cube_face.t)
      ~(desired_coordinates : Cube_face.t)
      ~side_length
      { Pos.row; column }
    =
    let row_adjustment, column_adjustment =
      coordinate_adjustment ~original_coordinates ~desired_coordinates
    in
    let compute_adjustment (sign, source) =
      let value =
        match source with
        | `Row -> row
        | `Column -> column
      in
      match sign with
      | `Flip -> -value
      | `Same -> value
    in
    reset_coordinates
      { row = compute_adjustment row_adjustment
      ; column = compute_adjustment column_adjustment
      }
      ~side_length
  ;;

  module Preprocessed = struct
    type t =
      { right : Pos.t Pos.Map.t
      ; left : Pos.t Pos.Map.t
      ; up : Pos.t Pos.Map.t
      ; down : Pos.t Pos.Map.t
      ; walls : Pos.Set.t
      ; top_left_free : Pos.t
      }
    [@@deriving sexp_of, compare]
  end

  let top_left_free (input : Input.t) =
    Array.find_mapi_exn input.grid ~f:(fun row line ->
        Array.find_mapi line ~f:(fun column -> function
          | None | Some `Wall -> None
          | Some `Open -> Some (Pos.of_indices ~row ~column)))
  ;;

  let walls (input : Input.t) =
    Array.foldi input.grid ~init:Pos.Set.empty ~f:(fun row walls line ->
        Array.foldi line ~init:walls ~f:(fun column walls -> function
          | None | Some `Open -> walls
          | Some `Wall -> Set.add walls (Pos.of_indices ~row ~column)))
  ;;

  let array_foldi_right array ~init ~f =
    let start = Array.length array - 1 in
    Array.fold_right array ~init:(start, init) ~f:(fun v (i, init) -> i - 1, f i init v)
    |> snd
  ;;

  let compute_horizontal ~line_foldi (input : Input.t) =
    Array.foldi input.grid ~init:Pos.Map.empty ~f:(fun row next_steps line ->
        let minimum, last, next_steps =
          line_foldi
            line
            ~init:(None, None, next_steps)
            ~f:(fun column (minimum, previous, next_steps) -> function
            | None -> minimum, previous, next_steps
            | Some (`Open | `Wall) ->
              let current_position = Pos.of_indices ~row ~column in
              let minimum =
                match minimum with
                | None -> Some current_position
                | Some _ -> minimum
              in
              let next_steps =
                match previous with
                | None -> next_steps
                | Some previous -> Map.set next_steps ~key:previous ~data:current_position
              in
              minimum, Some current_position, next_steps)
        in
        Map.set next_steps ~key:(Option.value_exn last) ~data:(Option.value_exn minimum))
  ;;

  let compute_right = compute_horizontal ~line_foldi:Array.foldi
  let compute_left = compute_horizontal ~line_foldi:array_foldi_right

  let compute_vertical ~column_fold (input : Input.t) =
    let minimums, previous, next_steps =
      column_fold
        input.grid
        ~init:(Int.Map.empty, Int.Map.empty, Pos.Map.empty)
        ~f:(fun row (minimums, previous, next_steps) line ->
          Array.foldi
            line
            ~init:(minimums, previous, next_steps)
            ~f:(fun column (minimums, previous, next_steps) -> function
            | None -> minimums, previous, next_steps
            | Some (`Open | `Wall) ->
              let current_position = Pos.of_indices ~row ~column in
              let minimums =
                match Map.add minimums ~key:column ~data:current_position with
                | `Duplicate -> minimums
                | `Ok minimums -> minimums
              in
              let next_steps =
                match Map.find previous column with
                | None -> next_steps
                | Some previous -> Map.set next_steps ~key:previous ~data:current_position
              in
              minimums, Map.set previous ~key:column ~data:current_position, next_steps))
    in
    Map.fold previous ~init:next_steps ~f:(fun ~key:column ~data next_steps ->
        Map.set next_steps ~key:data ~data:(Map.find_exn minimums column))
  ;;

  let compute_down = compute_vertical ~column_fold:Array.foldi
  let compute_up = compute_vertical ~column_fold:array_foldi_right

  let preprocess input =
    { Preprocessed.top_left_free = top_left_free input
    ; walls = walls input
    ; right = compute_right input
    ; left = compute_left input
    ; up = compute_up input
    ; down = compute_down input
    }
  ;;

  let movement_map (preprocessed : Preprocessed.t) ~direction =
    match (direction : Direction.t) with
    | Up -> preprocessed.up
    | Down -> preprocessed.down
    | Left -> preprocessed.left
    | Right -> preprocessed.right
  ;;

  let rec simulate_action ~walls ~movement_map ~direction ~position = function
    | Action.Rotate rotation -> Direction.rotate direction rotation, position
    | Step n ->
      if n = 0
      then direction, position
      else (
        let next_position = Map.find_exn movement_map position in
        if Set.mem walls next_position
        then direction, position
        else
          simulate_action
            ~walls
            ~movement_map
            ~direction
            ~position:next_position
            (Step (n - 1)))
  ;;

  let simulate_actions preprocessed actions =
    List.fold
      actions
      ~init:(Direction.Right, preprocessed.Preprocessed.top_left_free)
      ~f:(fun (direction, position) action ->
        simulate_action
          ~walls:preprocessed.walls
          ~movement_map:(movement_map preprocessed ~direction)
          ~direction
          ~position
          action)
  ;;

  module Part_1 = struct
    include Int_result

    let run input =
      let direction, pos = simulate_actions (preprocess input) input.actions in
      Direction.score direction + ((pos.row + 1) * 1000) + ((pos.column + 1) * 4)
    ;;
  end

  module Part_2 = Framework.Unimplemented
end

let () = Framework.register ~day:22 (module Day_22)
let link () = ()
