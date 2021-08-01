### Haskell repo

- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
- `::` is read as `"has type of"`
- `Explicit types` are always denoted with the first letter in `capital case`.
- Because it's not in `capital case` it's actually a `type variable`. That means that a can be of any type.
- Functions that have `type variables` are called `polymorphic functions`.
- Everything before the `=>` symbol is called a class constraint.
- `:t (==)`, results in
- `(==) :: Eq a => a -> a -> Bool`
- We can read the type declaration like this: the equality function `takes any two values` that are of the same type and returns a Bool. The type of those two values must be `a member of the Eq class` (this was the `class constraint`).
- All standard Haskell types except for `IO` (the type for dealing with input and output) and `functions` are `a part of the Eq typeclass`.
- `foldl'`, `foldl1'`:
  - `foldl'` and `foldl1'` are stricter versions of their respective lazy incarnations. When using lazy folds on really big lists, you might often `get a stack overflow error`. The culprit for that is that due to the lazy nature of the folds, the accumulator value isn't actually updated as the folding happens. What actually happens is that the accumulator kind of makes `a promise` that it will compute its value when asked to actually produce the result (also called `a thunk`). That happens for every intermediate accumulator and all those `thunks` overflow your stack. The strict folds aren't lazy buggers and actually compute the intermediate values as they go along instead of filling up your stack with `thunks`. So if you ever get stack overflow errors when doing lazy folds, try switching to their strict versions.
- Set:
  - We can check for `subsets` or `proper subset`:
    - Set A is a `subset` of set B if B contains all the elements that A does. 
    - Set A is a `proper subset` of set B if B contains all the elements that A does but has more elements.