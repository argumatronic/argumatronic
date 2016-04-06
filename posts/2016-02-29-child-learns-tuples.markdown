---
title: Today My Son Learned About Tuples
---

> I've been working on [Haskell Programming from First Principles](http://haskellbook.com/). My son and I started an experiment to see how well he, a fifth grader who didn't know much programming or algebra, could teach himself Haskell using that book. He reads the book, does exercises, asking questions where he needs to. I answer his questions and review with him what he has learned, to make certain it's sinking in. Periodically, I write about the experience for those who might be interested. The first post about this experience is [here](http://argumatronic.com/posts/2015-04-08-Haskell-to-10yo.html). I'll try to start updating more often. 

It's been so long since the last time I wrote about my son's experiences learning Haskell, I feel a bit of catching up is in order. He started last April, so you'd think he'd have learned more by now than he has. But last summer our life took some strange turns and went absolutely chaotic for a few months, and we didn't get settled down and into a new school routine until after Christmas. He's been working on learning Haskell as part of his school work since January.

I had him start the book over. We had added the lambda calculus chapter since the last time he read the book, and I felt strongly that he should try to work through it. He had a few mild issues with terminology, but he was able to do all the exercises correctly except one. That one he completed once we worked through the point where he'd become confused. He's a smart enough kid, who loves logic problems and math, that's true, but I also just don't think the principles of the lambda calculus are as difficult as we are often led to believe.

There hadn't been much else to report. He got through the first couple of Haskell chapters without anything exciting happening. Today, though, he was in chapter 4 (Basic Datatypes) and was introduced for the first time to tuples.

I wanted to make sure he understood the differences between tuples and lists. He's not yet at the point where he can read datatypes well, although he has the basic idea of type variables. So, he understands that `[a]` means all the elements of the list must be the same type, because there's only one variable. I checked to make sure he understood that the presence of two different variables in `(a, b)` means that the two types *can* differ but do not *necessarily* differ. So, we fired up GHCi and played with that a bit.

Out of curiosity, I then had him start playing with the `length` function. I get the feeling that the result of 

```haskell
length [1, 2, 3]
```

is fairly obvious to most people, so long as they have the sense that's a list and `length` will count the elements of the list.

I wanted to get a feel for his intuitions about tuples, though, so after doing some examples with lists, I had him type

```haskell
length (1, 3)
```

and asked him what he thought the result would be. He hesitated slightly, then guessed that it would return 1.

It does.

But, you know, he doesn't know about higher-kinded types. And he doesn't know about `Functor` and `Foldable`. There's no real way he could yet. So I asked him why he thought that, to understand his reasoning.

He said that in his understanding a tuple is one value; there are two *items* within the tuple, but it's really one value. He entered as evidence

```haskell
Prelude> length [(1, 2), (2, 4), (5, 6)]
3
```

If that list has 3 values, then each tuple is one. 

It's solid reasoning. In this case, it's not quite the correct reason why the length of a pair is 1 -- though, here, there is something intuitive about that, is there not? *The length of a pair is 1, because it is one pair.*

Of course, there are times when a list is also one value, such as 

```haskell
Prelude> length (Just [5, 6, 7])
1
```

when it is the `a` of a `Just a`, or if you had a list of lists, like the list of tuples above. 

Still, it was enjoyable watching him reason through this. I can't wait until he gets to the Functor chapter of the book and finds out the real reason his answer was correct. 
