# LambdaTrader

A terminal chatbot, written in Haskell, that understands plain-English requests about stocks and turns them into structured queries against a live finance API — with a persistent virtual portfolio and long-term memory.

```
λ> I bought 10 shares of Apple
Got it! You now own 10 stocks of Apple at an average price of $211.50

λ> What's the stock price of Tesla?
The stock price of Tesla is $318.42

λ> How is my portfolio doing?

Here is your Portfolio Performance:

Apple | Buy Price: $211.50 | Current Price: $214.02 | Percentage Change: ↑ 1.19% | Total Profit / Loss: +$25.20

λ> What do analysts think of Microsoft?
Analysts are generally positive, think it's worth buying

λ> Remember that my favourite stock is Nvidia
Okay.

λ> What do you know about my favourite stock?
Sure - my favourite stock is Nvidia.
```

## Why

Parsing free-text input into a well-typed set of commands is a nice small proving ground for combinator parsing: instead of pattern-matching on raw strings throughout the app, every request is parsed once into a closed `Request` ADT and handled exhaustively from there. It started as a CS141 university assignment (the "parse text into a chatbot" skeleton) and has since been extended with real stock market data, portfolio tracking, and persistent state.

## Features

- **Natural language parsing** (via `megaparsec`) — many phrasings map to the same request, e.g. "I bought", "I purchased", "I acquired" all resolve to the same buy action.
- **Live market data** — quotes, company profiles, analyst recommendations, and recent news, pulled from the [Finnhub](https://finnhub.io/) API.
- **Virtual portfolio** — buy/sell shares, track weighted average cost basis, and see performance (profit/loss, % change, best/worst holding) against live prices.
- **Persistent memory** — named facts ("remember that ..."), portfolio state, and your API key are saved to disk between sessions.
- **Simple arithmetic evaluator** with a "that" back-reference to the last result (e.g. "What is 4 plus 5?" then "What is that times 2?").

## Getting started

### Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/README/) (GHC is managed automatically by Stack)
- A free API key from [Finnhub](https://finnhub.io/register)

### Build & run

```bash
stack build
stack run
```

On first run, set your API key from inside the chatbot:

```
λ> Set API key to <your-finnhub-key>
```

The key is persisted to `data/memory.json` so you only need to do this once.

### Tests

```bash
stack test
```

## Architecture

The codebase is split so that natural language never touches business logic directly — everything flows through a typed `Request`:

```
user input → Parser.hs → Request.hs (ADT) → Responder.hs → API.hs / Memory.hs
```

| Module | Responsibility |
|---|---|
| `TUI.hs` | REPL loop: reads a line, parses it, dispatches to the responder, forever. |
| `Parser.hs` | Megaparsec combinators that turn free text into a `Request`. |
| `Request.hs` | The `Request`/`Expr` ADTs — every command the bot understands. |
| `Responder.hs` | Executes a `Request` against memory/API and prints the reply. |
| `API.hs` | All Finnhub HTTP calls and JSON decoding. |
| `Memory.hs` | Session state (portfolio, facts, cached articles, API key), persisted as JSON. |
| `Numbers.hs`, `Helpers.hs` | Shared utilities. |

## Example commands

```
Hello
What day is it?
What is 4 plus 5?
What is that times 2?
Remember that my favourite stock is Nvidia
What do you know about my favourite stock?
What's the stock price of Tesla?
How much are 10 shares of Apple?
Give me information about Microsoft
What do analysts think of Amazon?
Any news about Google
I want to know more about the number 3
I bought 10 shares of Apple
I sold 5 shares of Apple
Show me my portfolio
How are my stocks doing?
What is my portfolio worth?
What is my best performing stock?
```

## License

MIT — see [LICENSE](LICENSE).
