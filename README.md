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
  - `Don't think` of a function like `putStrLn` as a function that `takes a string and prints it to the screen`:
    - Think of it as a function that `takes a string and returns an I/O action`. That I/O action will, when performed, print beautiful poetry to your terminal.
- `Referential transparency`: a function, if `given the same parameters twice`, `must produce the same result twice`.
- `Randomness`:
  - `RandomGen` typeclass is for types that can act as `sources of randomness`.
  - `Random` typelcass is for things that can take on random values.
  - `random :: (RandomGen g, Random a) => g -> (a, g)` := `random` returns new `RandomGen` too
    - i.g. `random (mkStdGen 100)`: manurally make a random geneartor.
      - get other types other than Int, `random (mkStdGen 100) :: (Float, StdGen)`
- `reads`: `read` without throwing en exception.
  - samples:
    - `reads "1 2 3" :: [(Int, String)]                 -- [(1," 2 3")]`
    - `reads "(1,2) (3,4)" :: [((Int, Int), String)]    -- [((1,2)," (3,4)")]`
    - `reads "(1,2)(3,4)" :: [((Int, Int), String)]     -- [((1,2),"(3,4)")]`
    - `reads "(1,2)\n(3,4)" :: [((Int, Int), String)]   -- [((1,2),"\n(3,4)")]`
    - `reads "(1,2)    (3,4)" :: [((Int, Int), String)] -- [((1,2),"    (3,4)")]`
  - `reads` returns empty list when doesn't match:
    - `null $ (reads "aa" :: [(Int, String)]) -- True`
- `ByteString`:
  - ByteStrings are sort of like lists, only each element is one byte(or 8 bits) in size. The way they handle laziness i also different.
    - strict: completely do away with laziness. you can't have things like infinite list(because they're read into memory at once), but the upside is theres's less overhead becuase there are no thunks(the technical temr for promise)
    - lazy: they are stored in chuncks(not to be confused with thunks!), each chunk has a size of 64K. This is cool because it won't cause the memory usage to skyrocket and the 64K probably fits neatly into your CUP's L2 cache.
  - `Date.ByteString.Lazy.pack`: `pack:: [Word8] -> ByteString` takes list of bytes of type `Word8` and reutrns a `ByteString`, making it less lazy, so that it's lazy only at 64K intervals.
    - `B.pack [99,97,110] -- "can"`
  - `fromChunks` takes a list of strict bytestrings and converts it to a lazy bytestring. 
  - `toChunks` takes a lazy bytestring and converts it to a list of strict ones.
    - `B.toChunks $ B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48,49]] -- ["()*","+,-","./01"]`
- `Execption with pure functions?`:
  - Once pure functions start throwing `exceptions`, it matters when they are evaluated(although pure functions are lazy by default, which means that we don't know when they will be evaluated and that it shouln't matter). That's why we `can only catch exceptions thrown` from pure functions `in the I/O part of our code`. And that's bad, because we want to `keep the I/O part as small as possible`. However, if we don't catch them in the I/O part of our code, our program crashes. The solution? `Don't mix exceptions and pure code`. Take advantage of Haskell's powerful type system and use types like `Either` and `Maybe` to represent results that may have failed.
- Protip: it really helps to first think what the type declaration of a function should be before concerning ourselves with the implementation and then write it down. In Haskell, a function's type declaration tells us a whole lot about the function, due to the very strong type system.
- `Functor`:
  - `(->) r` is an instance of Functor
  - Not only is the function type `(->) r` a `functor` and an `applicative functor`, but it's also a `monad`. Just like other `monadic values`, `a function can also be considered a value with a context`. The context for functions is that that value is not present yet and that we have to apply that function to something in order to get its result value.
    - function's Monad instance is locaded in `Control.Monad.Instances`
  - You could write `(->) r` as `(r ->)`
    - `fmap :: (a -> b) -> (r -> a) -> (r -> b)`: we see that it takes `a function from a to b` and `a function from r to a` and returns `a function from r to b`. 
    - `instance Functor (r ->) where fmap f g = (\x -> f (g x))`:
      - `fmap (*3) (+100)`: `f := (*3)`, `g := (+100)`, therefore `fmap (*3) (+100) 1` results in `303`. same as `function comopsition` `fmap (*3) (+100) equals (*3) . (+100)`
- `Applicative`:
  - `((+) <$> (*2) <*> (+10)) 4`: apply 4 to each patially applied functions and then combine the results of each fully applied functions.
  - `Applicative functors laws`:
    - `pure f <*> x = fmap f x`
    - `pure id <*> v = v`
    - `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`
    - `pure f <*> pure x = pure (f x)`
    - `u <*> pure y = pure ($ y) <*> u`
- `newtype`:
  - When we use `newtype` to wrap `an existing type`(i.g., `[Char]`, `Int`, etc.), the type that we get is separate from the original type. If we make the following newtype: `newtype CharList = CharList { getCharList :: [Char] }`, WEe can't use `++` to put together a `CharList` and a list of type `[Char]`. We can't even use `++` to put together two `CharLists`, because `++` works only on `lists` and the `CharList` type isn't a list, even though it could be said that it contains one. We can, however, convert two `CharLists` to `lists`, `++` them and then convert that back to a `CharList`(by extarct with a method, in this case `getCharList`. this converts newtype to original type). 
- `data` vs `type` vs `newtype`:
  - If you just want your type signatures to look cleaner and be more descriptive, you probably want `type synonyms`. 
  - If you want to take an existing type and wrap it in a new type in order to `make it an instance of a type class`, chances are you're looking for a `newtype`, and 
  - If you want to make something `completely new`, odds are good that you're looking for the `data` keyword.
- `Monoids laws`:
  - ``mempty `mappend` x = x`` -- identity law
  - ``x `mappend` mempty = x`` -- identity law
  - ``(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)`` -- associative law

- [implementation-of-foldable-in-haskell](https://stackoverflow.com/questions/29295823/implementation-of-foldable-in-haskell)
  - You don't need the Monoid part at all(conversion from Int to Sum Monoid in add operation) - the default implementations work just fine(it's most likely not obvious `how foldr1 (+) can be expressed just in terms of foldMap`).
- [Foldable and Traversable](https://blog.jakuba.net/2014-07-30-foldable-and-traversable/)
- `Monads`:
  - `(>>=) :: (Monad m) => m a -> (a -> m b) -> m b`: `(>>=)`: bind 
    - takes a monadic value (that is, a value with a context) and feeds it to a function that takes a normal value but returns a monadic value(normal value `a` to convert fancy value `m b` with a function `a -> mb`).
  - `>>`: Instead of making functions that ignore their input and just return a predetermined monadic value, we can use `>>` function
    - `m >> n = m >>= \_ -> n` where type definition is `(>>) :: (Monad m) => m a -> m b -> m b`
  - In fact, `list comprehensions` are just syntactic sugar for using `lists as monads`. In the end, `list comprehensions` and `lists in do notation` translate to using `>>=` to do computations that feature non-determinism. 
  - `filtering in list comprehensions` is the same as using `guard`. 
  - `laws`:
    - `Left identity` and `right identity` are basically laws that describe how `return` should behave
      - `Left identity`: `return x >>= f is the same damn thing as f x`:
        - `return 3 >>= (\x -> Just (x+100000))` and `(\x -> Just (x+100000)) 3 ` are the same.
      - `RIght identity`: `m >>= return is no different than just m`
        - `return` puts a value in a minimal context that still presentst that value as its result.
        - `Just "move on up" >>= (\x -> return x) -- Just "move on up"`
        - `putStrLn "Wah!" >>= (\x -> return x) -- IO("Wah!")`
    - `Associativity`:
      - `Doing (m >>= f) >>= g is just like doing m >>= (\x -> f x >>= g)`: we have a chain of monadic function applications with `>>=`, `it shouldn't matter how they're nested`.
      - `return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2` and `return (0,0) >>= (\x -> landRight 2 x >>= (\y -> landLeft 2 y >>= (\z -> landRight 2 z)))` are the same.
      - `f <=< (g <=< h) `should be the same as `(f <=< g) <=< h`.

- `Inefficient list construction`:
  - When using the `Writer monad`, you have to be careful which `monoid` to use, because using lists can sometimes turn out to be very slow. That's because lists use `++` for `mappend` and using `++` to `add something to the end of a list` is slow if that list is really long. 