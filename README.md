## Stock market

Small program to try programming languages
- very fast to visually check is ok
- no external dependencies
- complex enough to give a decent idea of how the language behaves
- result looks cool


### It's a dumb program, so assumptions were made to make it simpler

- There will be always NUMBER_OF_OFFERS sell orders
- There will be always NUMBER_OF_OFFERS buy orders
- First 10 buy orders are random between 4 and 25
- First 10 sell orders are random between 25 and 50
- If final exchange price last day was higher than the previous one, buy twice
- If final exchange price last day was lower than the previous one, sell twice
- Every time someone buys, a new selling offer is made, at a higher price than the new sell price
- Every time someone sells, a new buying offer is made, at a lower price than the new buy price
- Bid-ask spread is random between 1 and 10

Example result:

![stock graph](stock-graph.png)


## TODO
- ✅ haskell
- ⬜️ prolog
- ⬜️ lisp
- ⬜️ elixir
- ⬜️ clojure
- ⬜️ nim
- ⬜️ c++
- ⬜️ nix
- ✅ rust
- ✅ Javascript
- ✅ Python
- ⬜️ Ruby
