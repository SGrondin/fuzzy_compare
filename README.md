fuzzy_compare
=============

Extremely fast Levenshtein comparator.

Checks whether 2 values are within a predetermined number of edits of one another.

- most comparisons take under 5 µs, depending on the length of the values
- a Functor is provided to enable comparisons across any arbitrary types
- string comparisons are provided (functorized) out of the box
- reuse the same automaton across all comparisons with the same `max_edits`, regardless of the type of the values being compared
- `max_edits` must be between `0` and `3` (inclusively) due to the astronomical scaling factor during graph building

### Example

```ocaml
(* Create an automaton *)
let within3 = Fuzzy_compare.create ~max_edits:3 in

(* true *)
let b1 = Fuzzy_compare.String.eval within3 "kitten" "kitsch" in

(* false *)
let b2 = Fuzzy_compare.String.eval within3 "kittens" "kitsch" in
```

### Install

```
opam install fuzzy_compare
```

### Unicode / arbitrary types

This example demonstrates how to compare unicode string as well as how to compare your own types.

For unicode we'll need `uunf` and `uuseg`:
```
opam install uunf uuseg
```

First apply the `Make` functor:
```ocaml
(* Fuzzy_compare.Make is documented in [fuzzy_compare.mli] *)
module Utf8 = Fuzzy_compare.Make (struct
  type t = string

  type cmp = string [@@deriving equal]

  type index = string array

  let index x =
    (* Normalize to Unicode NFC,
       then break the string into an array of Grapheme Clusters *)
    Uunf_string.normalize_utf_8 `NFC x
    |> Uuseg_string.fold_utf_8 `Grapheme_cluster (fun acc s -> s :: acc) []
    |> Array.of_list_rev

  let length = Array.length

  let get = Array.get

  let fold = Array.fold
end)
```

Then proceed as usual:
```ocaml
let within2 = Fuzzy_compare.create ~max_edits:2 in

(* false because each emoji is made of multiple bytes *)
let b1 = Fuzzy_compare.String.eval within2 "🏁🏴🚩" "🚩🏴🏁" in


(* true thanks to Unicode awareness *)
let b1 = Utf8.eval within2 "🏁🏴🚩" "🚩🏴🏁" in

(* false *)
let b2 = Utf8.eval within2 "🏁🏴🚩" "🚩🏳️" in
```

### Thanks

Thanks to Paul Masurel and Jules Jacob for explaining clearly the beauty of the algorithms contained within the "Fast String Correction with Levenshtein-Automata" paper, and thanks to Klaus Schulz and Stoyan Mihov for writing the paper.

This library implements the most sophisticated (and most efficient) version described by Paul Masurel, with several additional low level optimizations.

Recommended reading:
https://julesjacobs.com/2015/06/17/disqus-levenshtein-simple-and-fast.html
https://fulmicoton.com/posts/levenshtein/
