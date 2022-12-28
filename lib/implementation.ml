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

  module Part_2 = Framework.Unimplemented
end

let () = Framework.register ~day:10 (module Day_10)
let link () = ()
