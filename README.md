Purely functional blackjack in Haskell
======================================

A purely functional command-line blackjack game written in Haskell. Since card games
are usually implemented in a stateful manner I used monads and monad transformers to emulate mutable state in a safer way.

In order to build the program you have to run `stack setup` and `stack build` (this assumes that you have stack installed).
Then you can run the program with `stack exec blackjack-exe`.
