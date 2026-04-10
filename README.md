# Workout Tracker

A type-safe workout tracker written in Haskell. The project focuses on functional programming techniques rather than a large feature set — the goal is a clean, well-structured codebase that demonstrates strong typing, monadic parsing, and type-safe web APIs.

## Features

- Submit workouts using a simple DSL
- View past workouts and delete them
- Track volume progress per exercise over time
- Minimal web frontend served directly by the Haskell server

## DSL Syntax

Workouts are entered as plain text, one exercise per line:
Squat 5x100.0

Bench 10x80.0, 8x85.0, 6x90.0

Deadlift 3x180.0

Each line follows the format `Exercise RepsxWeight`, where multiple sets of the same exercise can be written as a comma-separated list.

## Project Structure
src/Workout/
Domain.hs         -- Core data types (Exercise, Set, Workout)
Parser.hs         -- MegaParsec DSL parser
Calculations.hs   -- HasVolume typeclass and volume calculations
PrettyPrint.hs    -- Pretty printer for roundtrip testing
API.hs            -- Servant type-level API definition
Server.hs         -- Request handlers and server wiring
App.hs            -- ReaderT/ExceptT monad transformer stack
app/
Main.hs           -- Entry point
test/
Main.hs           -- QuickCheck property tests
static/
index.html        -- Web frontend

## Techniques

- **Algebraic data types** to model exercises, sets, and workouts
- **Custom typeclass** (`HasVolume`) for polymorphic volume calculation
- **Monadic parsing** with MegaParsec — Applicative and Alternative style
- **Type-level API** with Servant — compile-time route and handler checking
- **Monad transformers** — `ReaderT AppEnv (ExceptT AppError IO)` for structured effects
- **JSON serialization** with Aeson
- **Property-based testing** with QuickCheck, including parser roundtrip tests

## Running

```bash
cabal run
```

Then open [http://localhost:8080](http://localhost:8080) in your browser.

## Running Tests

```bash
cabal test --test-show-details=direct
```

## Dependencies

- `servant` / `servant-server` — type-safe web API
- `warp` — HTTP server
- `megaparsec` — DSL parser
- `aeson` — JSON serialization
- `blaze-html` / `servant-blaze` — HTML serving
- `QuickCheck` — property-based testing
- `mtl` / `transformers` — monad transformer stack
- `containers` — `Data.Map` for progress aggregationSonnet 4.6