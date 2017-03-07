### Zip!

Above we saw that the list monoid applicatives rely on is concatenation, which is often considered the canonical monoid of lists. But, friends, there is another possibility: the cross-product. To get that monoidal behavior, though, we need a different type wrapper, called `ZipList` (imported from `Control.Applicative`).

Let's look at how the normal list applicative behaves:

```haskell
λ> pure (+) <*> [3, 10] <*> [4, 20]
[7,23,14,30]

λ> pure (+) <*> ZipList [3, 10] <*> ZipList [4, 20]
ZipList {getZipList = [7,30]}

λ> (,) <$> [1, 4] <*> [3, 7]
[(1,3),(1,7),(4,3),(4,7)]

λ> (,) <$> ZipList [1, 4] <*> ZipList [3, 7]
ZipList {getZipList = [(1,3),(4,7)]}

```


The explicit difference here is that we're using two different types, list versus ZipList. But the implicit difference, and in this case the difference that matters, is the way the types monoidally merge.
