---
title: Assignment 2 - Minzinc-Lite
author: FIT2102 Programming Paradigms
margin: 1inch
---

Please do not change the names of the functions defined in the Assignment.hs file. You may (and are highly encouraged) to implement your parsers **alongside** these pre-defined functions.

## Running the Code

```
$ stack test
```

This will generate the output files using the sample input MiniZinc files, by running your code.

All example MiniZinc files are stored within `examples/inputs` and the output of your parser will be saved in `examples/output`.

## Running the Interactive Page

In the Haskell folder run:

```
$ stack run
```

In a separate terminal, in the javascript folder run:

```
$ npm i
$ npm run dev
```

You can type in to the LHS of the webpage and inspect the generated solution.

## Part E: faster solving

In `Haskell/src/Assignment.hs` (see the comment above `search`):

- Forward checking (`forwardCheck`): after each assignment, every
  remaining variable's domain shrinks to its still-legal values; a
  branch stops as soon as any domain is empty.
- MRV ordering (`mostConstrainedFirst`): the variable with the fewest
  values left is solved next.
- Branch and bound (`bestSolution`): `maximize`/`minimize` walk the
  search tree once and cut every branch that cannot beat the best
  value so far.

Try it: paste this into the web page and click **Run**. It solves in
well under a second; plain backtracking takes close to a minute.

```
int: n = 12;
array[1..n] of var 1..n: q;

constraint allDifferent(q);
constraint allDifferent(i in 1..n)(q[i] + i);
constraint allDifferent(i in 1..n)(q[i] - i);

solve minimize q[1];
```
