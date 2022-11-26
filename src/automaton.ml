open! Core

(* https://fulmicoton.com/posts/levenshtein/ *)
(* https://julesjacobs.com/2015/06/17/disqus-levenshtein-simple-and-fast.html *)

type state = {
  offset: int;
  d: int;
}
[@@deriving sexp, compare, hash]

module StateSet = struct
  include Set.Make (struct
    type t = state [@@deriving sexp, compare]
  end)

  let hash_fold_t state set = fold set ~init:state ~f:[%hash_fold: state]

  let hash x = hash_fold_t (Hash.create ()) x |> Hash.get_hash_value
end

type node = {
  min_offset: int;
  states: StateSet.t;
  hash: int;
}

let make_node min_offset states = { min_offset; states; hash = [%hash: StateSet.t] states }

let empty_node = make_node 0 StateSet.empty

let sexp_of_node { min_offset; states; hash = _ } =
  Sexp.List
    [
      List [ Atom "min_offset"; [%sexp_of: int] min_offset ];
      List [ Atom "states"; [%sexp_of: StateSet.t] states ];
    ]

let node_of_sexp = function
| Sexp.List [ List [ Atom "min_offset"; sexp1 ]; List [ Atom "states"; sexp2 ] ]
 |Sexp.List [ List [ Atom "states"; sexp2 ]; List [ Atom "min_offset"; sexp1 ] ] ->
  make_node ([%of_sexp: int] sexp1) ([%of_sexp: StateSet.t] sexp2)
| sexp -> failwithf !"Invalid s-exp for node: %{Sexp}" sexp ()

module DFA = Int.Table

let enumerate_chi_values width = Array.init Int.(2 ** width) ~f:Fn.id

let make_chi_vector ~width blank = Array.create ~len:Int.(2 ** width) blank

let normalize = function
| states when StateSet.is_empty states -> empty_node
| states ->
  let min_offset =
    let { offset = init; _ } = StateSet.min_elt_exn states in
    StateSet.fold states ~init ~f:(fun acc { offset = x; _ } -> min x acc)
  in
  let shifted_states =
    StateSet.map states ~f:(fun state -> { state with offset = state.offset - min_offset })
  in
  make_node min_offset shifted_states

let fold_chi_slice ~width chi ~from ~init ~f =
  if from > width then failwithf "fold_chi_slice: %d >= %d. Please report this bug." from width ();
  let rec loop acc = function
    | i when i = width -> acc
    | i ->
      let acc = f (i - from) acc (chi land (1 lsl i) > 0) in
      (loop [@tailcall]) acc (i + 1)
  in
  loop init from

let transitions ~width { offset; d } chi =
  let init =
    if d > 0
    then StateSet.of_array [| { offset; d = d - 1 }; { offset = offset + 1; d = d - 1 } |]
    else StateSet.empty
  in
  fold_chi_slice ~width chi ~from:offset ~init ~f:(fun i acc -> function
    | false -> acc
    | true -> StateSet.add acc { offset = offset + i + 1; d = d - i })

let is_useful states = function
| { offset = _; d = d1 } when d1 < 0 -> false
| { offset = o1; d = d1 } ->
  StateSet.exists states ~f:(function
    | { offset = o2; d = d2 } when o1 = o2 && d1 = d2 -> false
    | { offset = o2; d = d2 } -> d2 - d1 >= abs (o1 - o2))
  |> not

let simplify states = StateSet.filter states ~f:(is_useful states)

let step ~width chi states =
  StateSet.fold states ~init:StateSet.empty ~f:(fun acc state ->
      transitions ~width state chi
      |> StateSet.fold ~init:acc ~f:(fun acc -> function
           | set when is_useful acc set -> StateSet.add acc set
           | _ -> acc))
  |> simplify

let initial_states ~max_edits = StateSet.singleton { offset = 0; d = max_edits }

type t = {
  max_edits: int;
  width: int;
  dfa: node array DFA.t;
}
[@@deriving sexp]

type folder = {
  last_hash: int;
  rest: StateSet.t list;
}

let create ~max_edits =
  if max_edits < 0 then failwithf "max_edits cannot be negative: %d" max_edits ();
  if max_edits > 3 then failwithf "max_edits cannot be more than 3: %d" max_edits ();
  let width = (3 * max_edits) + 1 in
  let chi_values = enumerate_chi_values width in
  let blank_chi_vector = make_chi_vector ~width empty_node in
  let dfa = DFA.create () in

  let rec loop = function
    | [] -> dfa
    | current_state :: rest ->
      let state_transitions = Array.copy blank_chi_vector in
      let { last_hash; rest; _ } =
        Array.fold chi_values ~init:{ last_hash = 0; rest } ~f:(fun { rest; _ } chi ->
            let new_states = step ~width chi current_state in
            let ({ states; hash; _ } as node) = normalize new_states in
            state_transitions.(chi) <- node;
            let rest =
              if DFA.mem dfa hash
              then rest
              else (
                DFA.add_exn dfa ~key:hash ~data:(Array.copy blank_chi_vector);
                states :: rest)
            in
            { last_hash = hash; rest })
      in
      DFA.set dfa ~key:last_hash ~data:state_transitions;
      (loop [@tailcall]) rest
  in
  let { states = norm_states; hash; _ } = normalize (initial_states ~max_edits) in
  DFA.add_exn dfa ~key:hash ~data:(Array.copy blank_chi_vector);
  { max_edits; width; dfa = loop [ norm_states ] }

module type Intf = sig
  type t

  type cmp [@@deriving equal]

  type index

  val index : t -> index

  val length : index -> int

  val get : index -> int -> cmp

  val fold : index -> init:'a -> f:('a -> cmp -> 'a) -> 'a
end

module type S = sig
  type u

  val eval : t -> u -> u -> bool
end

module Make (M : Intf) = struct
  let characteristic ~width query_index c offset =
    Fn.apply_n_times ~n:width
      (fun (i, acc) ->
        let acc =
          if offset + i < M.length query_index && M.equal_cmp (M.get query_index (offset + i)) c
          then acc lor (1 lsl i)
          else acc
        in
        i + 1, acc)
      (0, 0)
    |> snd

  let step_all { max_edits; width; dfa } query_index s_index =
    let init = normalize (initial_states ~max_edits) in
    M.fold s_index ~init ~f:(fun { min_offset = offset; states = _; hash } c ->
        let chi = characteristic ~width query_index c offset in
        let node = Array.get (DFA.find_exn dfa hash) chi in
        { node with min_offset = offset + node.min_offset })

  let eval automaton str1 str2 =
    let index1 = M.index str1 in
    let index2 = M.index str2 in
    match M.length index1, M.length index2 with
    | len1, len2 when abs (len1 - len2) > automaton.max_edits -> false
    | len1, len2 ->
      let query_index, query_len, s_index =
        if len1 > len2 then index1, len1, index2 else index2, len2, index1
      in
      let { min_offset; states; _ } = step_all automaton query_index s_index in
      StateSet.exists states ~f:(fun { offset; d } -> query_len - (offset + min_offset) <= d)
end

module String = Make (struct
  type t = string

  type cmp = char [@@deriving equal]

  type index = string

  let index x = x

  let length = String.length

  let get = String.get

  let fold = String.fold
end)
