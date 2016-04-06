---
title: Teaching a 10-year-old Haskell: Day One
---

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">Son: &quot;Well, I&#39;m going to keep learning JavaScript until you teach me Haskell.&quot;<a href="https://twitter.com/hashtag/ultimatum?src=hash">#ultimatum</a></p>&mdash; Gin and Julie (@argumatronic) <a href="https://twitter.com/argumatronic/status/522440942918586369">October 15, 2014</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

My older son, who is in fourth grade (homeschool) and just turned 10, has been nagging me for ages to teach him Haskell. The desire originates from me learning Haskell and writing [a book](http://haskellbook.com/) with Chris Allen. I have told him a little bit about Haskell as I’ve been learning it, and he thought it sounded cool. Today I finally gave in and started teaching him.

Now, he’s 10. He hasn’t really had any formal algebra instruction. He does have more previous programming experience than I had because he’s taken some JavaScript classes from Khan Academy and he’s taking the Minecraft Mod class from YouthDigital, and that uses Java.

We started today with running simple arithmetic expressions in GHCi and then also entering them into a text editor and loading them that way. We’re using the book that I’m writing as a guide, but because he’s 10 (and I’m here to teach him), I’m doing a lot of comprehension checks as we go through: “What’s the function name here? What’s the argument? What will the result be if you run this function with this argument? Cool, now run it and check your work.”

The book is intended for adults who may or may not have a teacher co-located with them, so we rely on a system of iterative deepening. That means we introduce concepts at places where it seems much too early, but just brush the surface of the concept. Later we return to it, and later we might return and review it again or go even deeper into it. So, as I’m working through the first part of chapter 1 with my son, there it is: All functions in Haskell take one argument and return one result. It seems puzzling there in the arithmetic chapter, and we don’t provide a lot of explanation there. I didn’t honestly think my son would understand that at all.

But he said, “But 1 + 2 looks like it has two arguments. Does that mean it’s two functions?”

Me: “Well, let’s think about that. At this point in the book we haven’t explained it. We just want you to think about it as you work through the arithmetic. How could that be? What could it mean?”

I wrote it out on paper at this point, first in the “normal” way 1 + 2, then explained that the function part is the `(+)` so let’s break that out: `(+) 1 2`.

Son: “Well. Hm. If the function is the `(+)`, then you first apply that to the 1. So…..you have something like `(1 +)`? And now you apply that to the 2? And then you get the final answer?”

Me: “Yeah that’s the right idea. Here, write it out: `(+) 1 2 = (+ 1) -> (1+)  2 = 3`”

I thought at this point since we were “thinking” about it, I’d deviate from the book a bit more and show him the type signature for `(+)`.

```haskell
(+) :: Num a => a -> a -> a
```

At this point, his entire knowledge of types was “they’re a way of classifying values” and he knew nothing about typeclasses. He intuited that the “Num” probably meant it was telling the variables that they had to be numbers, and I pretty much left it at that–I think that’s a good understanding of it for day one. I explained the typeclass arrow (=>) and we looked at the function definition. He noticed that there is no equals sign and each a just points to another a until you reach the end. We talked about how this confirms the “one argument, one result” pattern: “You take the function and apply it to the first a which gives you the second a, then you apply the function to the second a and get the result.” That’s the 10-year-old’s definition of currying.

We moved on and ran more stuff through GHCi, playing with different functions. He got pretty hung up on `div`/`mod` and `quot`/`rem`. He’s OK with fractional division and fine with the other division functions when they involve positive values, but at his current level of arithmetic, he doesn’t have much experience with negative numbers, and using `mod` with negative numbers was confusing the heck out of him so I told him we’d return to it later.

The other point we spent a lot of time on today was the idea of abstracting patterns to write reusable functions. This was another place where I thought he’d have a harder time, because, again, at his age, he doesn’t have much experience with it. But I showed him a bunch of expressions like, (1 + 2) * 3; (2 + 3) * 3; (3 + 3) * 3 and so on and asked him what’s the common thing. Well, it’s multiplying by 3 of course. So, I asked him, how could we make a function so that we could just input whichever addition expression we wanted to use and it would BADABING multiply it times 3 for us?

He thought. He looked ahead in the book a bit. He said tentatively, “well, could you use a variable?”

So, yes, we could and did.

I again thought it would trip him up when we started doing things like this:

```haskell
let double x = x * 2
double (double (1 + 2))
```

But he had no trouble with that at all and enjoyed putting in ever larger numbers to see if it would noticeably affect the speed of evaluation. Fun times.

Then I sort of dumped the book material about using let expressions and where clauses in his lap with a bunch of exercises and he did them on his own. I kept asking if he needed help, and he’d say no, and he just kept at it until he finally got too jealous of the fact that his brother was playing Minecraft that he wanted to stop doing exercises.

This was a couple of hours ago. I wanted to check to make sure he had really understood what he learned earlier–maybe I was asking leading questions in such a way that I only thought he understood currying. So I just gave him a pop quiz over dessert (the dessert makes the pop quiz go down easier for sure). He had a hard time finding the words to explain `Num a => a -> a -> a` but had an easy time explaining how `(2 * 3)` really is two functions, each with one argument and one result, and he drew it out on paper nicely. I think he feels less confident when it’s a bunch of variables–he’s not sure how to express what’s happening in words yet, so doing it visually is easier. It was funny, though, because I at first showed him just that type signature and asked him, “what’s this?” And he goes, “Well, it could be addition or multiplication but I can’t tell. Am I supposed to be able to tell?” No, son, your mom just phrased the question badly.

I think this was a pretty productive first day of Haskell learning. I expected him to have a harder time understanding it than he is. I have believed for a while that Haskell doesn’t have to be hard, that it’s usually just taught poorly. But I admit that I had some hesitation about introducing some of the underlying concepts of Haskell so early in the book. I’m feeling much better about that after today. Yes, he had a teacher here with him, but he’s also 10 and hasn’t had any algebra yet.