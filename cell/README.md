# chia/cell

Cell is a library for interactive, asynchronous computation in ClojureScript.

Start by checking out the [intro](https://www.maria.cloud/gist/f2bdc26bcb074bbc20b0b36fff114924) (in progress).

More examples:

- [Fetch, advanced example](https://www.maria.cloud/gist/9090884f3b4add7d744e428170e26759)
- [Cell Views](https://www.maria.cloud/gist/1a561967af1238fa5fd391c7925d364b)
- [Making a Ratom out of cells](https://www.maria.cloud/gist/67597f7660600dbdc52eaad6696e46c3)

### Motivating use cases

- Fetch data from a URL
- Perform an action on a timer/interval
- React to user input
- Smooth dataflow experience: cells can reference other cells, and changes propagate through the system

### Notes about cell identity

When using `defcell`, a cell's identity is equal to its fully qualified name (as a keyword).

When using `cell`, a cell's identity is composed of:

    [1] Current View + [2] Lexical Position + [3] (optional) :key prop

The combination of 1 & 2 mean that each instance of `(cell ...)` will result in at least one unique cell.
By adding a unique :key (3), it becomes possible to use cells within loops and sequentially-applied functions,
with one cell created per unique key passed to the cell.

### Comparison with React Hooks

A React hook's identity is composed of one thing: its evaluation order. This makes some things easy and other
things difficult. For example, hooks cannot be used within conditional branches or loops.
Every hook must be called exactly once and in the same order on every render.

The advantage of hooks is that, when the "rules of hooks" are followed, hooks require less bookkeeping
and conscious thought given to memoization. It is easy to write composable hooks without worrying about
the problem of correctly expressing "uniqueness" for the given domain. Reusable hook functions can be
written which do not know about each other, and it is not possible for keys to clash when rules are followed.

Cell takes a more explicit approach. The "rule of cells" is that if you want to return a cell from a function,
or otherwise create a cell in a loop, you must provide a globally unique :key. One key == one cell.