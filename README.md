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
- `Own Types and Typeclasses`:
 - `data  Bool = False | True`:
   - `data` means that we're defining a new data type.
   - `value constructors`: he parts after the `=`.
   - `data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647`:
     - `The first and last value constructors are the minimum and maximum possible values of Int`. It's `not` actually defined like this, the ellipses are here because we omitted a heapload of numbers, so this is just for illustrative purposes.
- `Do we benefit from type parameter in data declaration(meaming adding a typeclass constraint onto a parameter)?`:
  - If we were defining a mapping type, we could add `a typeclass constraint` in the data declaration:
  `data (Ord k) => Map k v = ...  `
  - However, it's a very strong convention in Haskell to `never add typeclass constraints in data declarations`. Why? Well, because we don't benefit a lot, but we end up writing more class constraints, even when we don't need them. If we put or don't put the `Ord k constraint` in the data declaration for `Map k v`, we're going to have to put the constraint into functions that assume the keys in a map can be ordered. But if we don't put the constraint in the `data declaration`, we don't have to put `(Ord k) =>` in the `type declarations` of functions that don't care whether the keys can be ordered or not. An example of such a function is toList, that just takes a mapping and converts it to an `associative list`. Its type signature is `toList :: Map k a -> [(k, a)]`. If `Map k v` had a type constraint in its data declaration, the type for toList would have to be `toList :: (Ord k) => Map k a -> [(k, a)]`, even though the function doesn't do any comparing of keys by order.

- We can `derive instances for the Ord type class`, which is for types that have values that can be ordered. If we compare two values of the same type that were made using different constructors, the value which was made with a constructor that's defined `first is considered smaller`. For instance, consider the `Bool` type, which can have a value of either `False` or `True`. Defining like `data Bool = False | True deriving (Ord)` makes `True` is bigger than `False`(`GT`)
- `data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum)`
  - Because all the value constructors are `nullary` (`take no parameters, i.e. fields`), we can make it part of the `Enum` typeclass. The Enum typeclass is for `things that have predecessors and successors`. We can also make it part of the `Bounded` typeclass, which is for things that have `a lowest possible value and highest possible value`.
  - `minBound :: Day  -- Monday`
  - `Saturday > Friday -- True`
  - `succ Monday -- Tuesday`
  - `[Thursday .. Sunday], [minBound .. maxBound] :: [Day]`

- Either: 
  - `Either a b`:
    - `a` is some sort of type that can tell us something about `the possible failure` 
    - `b` is the type of `a successful computation`

- `Fixity declaration`: When we define functions as `operators`, we can use that to give them a `fixity` (but we don't have to). A `fixity` states `how tightly the operator binds` and whether it's `left-associative` or `right-associative`. For instance, `*`'s fixity is `infixl 7` and `+`'s fixity is `infixl 6`. That means that they're both `left-associative` (4 * 3 * 2 is (4 * 3) * 2) but `* binds tighter than +`, `because it has a greater fixity`, so 5 * 4 + 3 is (5 * 4) + 3
  - When deriving `Show` for our type, Haskell will still display it as if the constructor was `a prefix function`, hence the parentheses around the operator (`remember, 4 + 3 is (+) 4 3`).
  - Normal prefix constructors or stuff like `8` or `'a'`, which are basically constructors for the `numeric` and `character` types, respectively.

- `Typeclasses`:
  - If we have say `class Eq a where` and then define a type declaration within that class like `(==) :: a -> -a -> Bool`, then when we examine the type of that function later on, it will have the type of `(Eq a) => a -> a -> Bool.`
  - `the minimal complete definition` for the typeclass, means the minimum of functions that we have to implement so that our type can behave like the class advertises.
    - We'd have to implement both of these functions(`==`, `\=` in `Eq`) when making a type an instance of it, because Haskell wouldn't know how these two functions are related. `The minimal complete definition` would then be: both `==` and `/=`.
  - `subclassing typeclasses of other typeclasses`:
    - The example of `Num` typeclass: `class (Eq a) => Num a where  ...`
      - `a` has to be `Eq` before it becomes `Num` (`a` has to be a `concrete typ`e, meaning `type constructors` like `Maybe` can't be sit in the spot alone.)
    - `instance (Eq m) => Eq (Maybe m) where ...`: 
      - We say this: we want all types of the form `Maybe` m to be part of the `Eq` typeclass, but only those types where the `m` is also a part of `Eq`

- `Kinds`:
  - Types have their own little labels, called `kinds`. A kind is more or less the `type of a type`
  - `*`(called star, or type): a concrete type. a type that doesn't take any type parameters and values can only have types that are concrete types.
  - We used `:k` on a `type` to get its `kind`, just like we can use `:t` on a `value` to get its `type`. Like we said, `types` are the `labels of values` and `kinds` are the `labels of types` and there are `parallels between the two`.
- Haskell's mechanism for poralizing `side-effects` and `function purity`:
  - Haskell actually has a really clever system for dealing with functions that have `side-effects` that `neatly separates the part of our program that is pure and the part of our program that is impure`, which does all the dirty work like talking to `the keyboard` and `the screen`. With those two parts separated, we can still reason about our pure program and take advantage of all the things that `purity` offers, like `laziness`, `robustness` and `modularity` `while efficiently communicating with the outside world`.
- `()`: `empty tuple` known as `unit`:
  - `:t putStrLn`: `putStrLn :: Strin -> IO ()`