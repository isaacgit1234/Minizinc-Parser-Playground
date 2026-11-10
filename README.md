# Minizinc-Parser-Playground

A full-stack web app that reads models written in a subset of MiniZinc (a
constraint-programming language), solves them, and shows the answer in the
browser.

<img width="1134" height="989" alt="image" src="https://github.com/user-attachments/assets/24da01b5-a45b-41c6-bdb8-cf30c2953b20" />

**Backend:** Haskell · **Frontend:** TypeScript, RxJS, Vite

## What it does

You type a model like this into the web page:

```
int: n = 4;
array[1..n] of var 1..n: q;
constraint allDifferent(q);
solve satisfy;
```

The backend parses it, checks it, finds a solution, and returns the values
of every variable. Models with `maximize` or `minimize` return the best
solution.

## Architecture

```
Browser (TypeScript + RxJS)  --POST /api/minizinc-->  Haskell server (Scotty)
                              <-----JSON result------   parse -> validate -> solve
```

## Backend (Haskell)

- **Parser built from scratch** using parser combinators. `Parser` is a
  newtype with hand-written `Functor`, `Applicative`, `Monad` and
  `Alternative` instances (`src/Instances.hs`).
- **Grammar:** integer expressions with correct precedence (right-associative
  `^`, unary minus, `*` `/`, `+` `-`), ranges and finite sets, comparison
  constraints, the `sum`, `min`, `max` and `allDifferent` macros, and
  `solve satisfy / maximize / minimize`.
- **Validation after parsing:** rejects duplicate declarations and names used
  before they are declared, with a parse error.
- **Solver** (`src/Assignment.hs`), with three optimisations:
  - *Forward checking:* after each assignment, remaining domains shrink to
    their still-legal values, and a branch stops as soon as a domain is empty.
  - *MRV ordering:* the variable with the fewest values left is solved next.
  - *Branch and bound:* `maximize` / `minimize` walk the search tree once and
    cut every branch that cannot beat the best value so far.
- On a 12-queens model, this solves in well under a second, where plain
  backtracking takes close to a minute.
- Automated tests run every model in `examples/inputs` (`test/Spec.hs`).

## Frontend (TypeScript)

- A single-page editor with **Run** and **Save** buttons.
- State is managed with RxJS streams (`fromEvent`, `scan`, `switchMap`) rather
  than mutable variables, and requests are sent with `fromFetch`.

## Run it

Backend, from the `Haskell` folder (listens on port 3000):

```
stack run
```

Frontend, in a second terminal, from the `JS` folder:

```
npm i
npm run dev
```

Run the tests with `stack test` in the `Haskell` folder.

## Credits

Built as an assignment for FIT2102 Programming Paradigms at Monash University.
The project template, example inputs and web-server scaffolding were provided
by the unit. The parser, the solver and the frontend logic are my own work.
