# Alexis
A library for writing lexers using regex rules.

Alexis is powered by [DFAs](https://en.wikipedia.org/wiki/Deterministic_finite_automaton) which makes inference approximately a single-pass operation.

# Getting Started
## Overview
Alexis provides a functor which requires
```
type token

val token_dfa : token Regex_dfa.t
```
and generates a lexer that outputs a `token` list on success.
## Demo
Check out `examples/` for some toy lexers built using Alexis!

You can also try out the `lexcaster_example_bin` binary, a repl for the lexers in `examples/`:

https://github.com/user-attachments/assets/a3d0542b-29d2-46b2-b072-c44faa71a370

```
dune exec bin/alexis_lexcaster_example_bin.exe
```

*note: whitespace is treated as a special character that is skipped between lexed tokens*

## Making a `Regex_dfa`
### Regex Configs
Regex configs are recursively defined with the following type signature:
```
type t =
  | Empty
  | Epsilon
  | Char of char
  | Concat of t * t
  | Or of t * t
  | Star of t
```
*(`Epsilon` is the empty string and `Empty` doesn't match any input)*

Alexis also exposes some common constructors for creating these configs:
```
val plus : t -> t
val opt : t -> t
val exact : string -> t
val char_or : char list -> t
val concat : t list -> t
val or_ : t list -> t
```
*i.e. `exact` matches an exact string; `concat` matches a sequence of regexes; etc.*

Each regex config matches a **single token**.

### Regex DFAs

A single regex config generates a single DFA which accepts if the regex config matches the input.
```
val create : ?priority:int -> Config.t -> cont_of_match:(Buffer.t -> 'a) -> 'a t
```
The first thing to note is `cont_of_match` -- this is a continuation that is invoked on a successful match. It takes a `Buffer.t` because it might be the case that the matched input is required to generate the associated token.

*For example, if you want to match a keyword, you probably won't need the buffer and can just apply `Fn.const`. On the other hand, if you want to match a string, you'll need the actual contents of the string to create the token.*

The second thing to note is the `priority` optional argument. It is necessary to understand the lexer mechanism before this parameter can be explained.
```
val merge_list : 'a t Nonempty_list.t -> 'a t
```
Notably, a lexer is a collection of DFAs. It does a sliding window (current buffer) which continues to expand *as long as there is at least 1 DFA that is not in a dead state*. In other words, it tries to match the current buffer to a single DFA (and then convert it into the associated token via `cont_of_match`). If the current buffer results in every DFA being in a dead state, it must retrieve the (cached) last state where at least 1 DFA was in an accept state (otherwise, lexing fails due to bad input).

The `priority` argument (default=`1`) is used in situations where multiple DFAs end up in an accepting state -- highest `priority` DFA is chosen.

This is especially useful for lexing freeform tokens (i.e. variable names) which should be set to **low** priority so that keywords are chosen ahead of them.

*For instance, `true` is a valid variable name in the sense that it starts with an alpha and contains only alphanumerics, but it should definitely match to a `bool`.*

# Implementation Details
## Core Concepts
### DFA Core
Foundationally, every DFA node is a total function from `char` to another DFA node. Additionally, every node is either an accept node or a fail node -- if the state by <eof> is accept, Alexis will invoke that node's continuation (`cont_of_match`). Without changing semantics, Alexis replaces the concept of fail nodes with incomplete and dead nodes -- where dead nodes are just sink nodes. This allows the optimization of exiting early instead of having to read the entire input.

### DFA Construction
As implied by the API, Alexis generates a DFA using a regex config. It does so by (fundamentally) mapping the universe of regex configs to a DFA node and minting edges between nodes via [Brzozowski derivative](https://en.wikipedia.org/wiki/Brzozowski_derivative) application. Intuitively, the derivative is a function applying a `char` to the regex config that outputs another regex config representative of a partial inference on the input regex config.

One small optimization note: applying the derivative on the entire universe of characters per regex config (node) is computationally expensive, so Alexis first generates the "next char" universe of that regex and iterates that set.

### Merge
Merge is a key part of the Alexis algorithm. Essentially, it collapses a set of DFAs (for each token) into a single mega-DFA. This is what drives the approximately single-pass inference performance -- the lexer only needs to feed the input into this machine rather than to those of each individual token.

This is done via a "threading" algorithm. Mathematically, the combiner (function of 2 DFAs) takes the cross product of the nodes between its inputs. Intuitively, the combined inference is like performing inference on each individual DFA and assigning the current node to be the pair of nodes in each DFA.

If:
- both are incomplete nodes -> combined node is incomplete
- only 1 is an accept node -> combined node is an accept node with the same continuation as the accept input node
- (at least) 1 is a dead node -> combined is the other node
- both are accept nodes -> combined node is an accept node whose continuation is that of the input node **with the highest priority**

## Performance

DFA inference is a single-pass algorithm. However, the Alexis lexer continues to perform partial (per-character) inference until it reaches a dead state, **and then backtracks to the last accept state**. In practice, most inputs and grammars shouldn't run into bad versions of this (where this practically dominates the computation time).

### Degerate Edgecase Example
- Token Foo: matches on [a]
- Token Bar: matches on [aa...aab]
- Input: [aa...aa]

Alexis will read the whole input because [Bar] is still matchable until the very end. After reaching <eof>, Alexis will backtrack to index 0 (which is the last accept state -- accepting Foo). At this point, the lexer will continue iteration from index 1 (and repeat the same process) resulting in a quadratic operation.
