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
```
dune exec bin/alexis_lexcaster_example_bin.exe
```

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
Notably, a lexer is a collection of DFAs. It does a sliding window (current buffer) which continues to expand *as long as there is at least 1 DFA that is not in a fail state*. In other words, it tries to match the current buffer to a single DFA (and then convert it into the associated token via `cont_of_match`). If the current buffer results in every DFA being in a fail state, it must retrieve the (cached) last state where at least 1 DFA was in an accept state (otherwise, lexing fails due to bad input).

The `priority` argument (default=`1`) is used in situations where multiple DFAs end up in an accepting state -- highest `priority` DFA is chosen.

This is especially useful for lexing freeform tokens (i.e. variable names) which should be set to **low** priority so that keywords are chosen ahead of them.

*For instance, `true` is a valid variable name in the sense that it starts with an alpha and contains only alphanumerics, but it should definitely match to a `bool`.*
