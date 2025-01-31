# Haskell Number Classes

## Introduction
In this video, I want to discuss the numerical classes in Haskell, classes that
are used to represent numbers. I think the general consensus is that the class
hierarchy used for numbers in Haskell is mostly historical, and if it were
redone now, it would probably be done in a better and more systematic way. At
the same time, any change to a part of the language that is so widely used
everywhere would also break an amazing amount of code, which is why people are
reluctant to change it. However, I take into account that things are not really
as good as they could be.

## Overloaded Literals
Some things are nice. For starters, something that we have already seen a number
of times is that literals, literal numbers, are actually overloaded themselves.
If you just type a number literal, then you get something that is actually
overloaded over the `Num` class. Again, we have the situation with the target
type being overloaded, so it's the context in which we use something that
affects what type this is actually going to be. This means that we can use a
literal like `2` in a context where we expect a `Double`, and then we get a
floating-point number. We can also see that the `Show` instance for `Double`
always prints a dot and a zero, even if we have something that happens to be
falling into the integral subset of the doubles.

If we say that this is in a context where this is an `Integer`, then it can be
used as an `Integer`. A similar thing applies to fractional literals. If we also
use a fractional literal, we won't get a type error that says this is a
`Double`; it will say that this is anything that belongs to the `Fractional`
class. There are still different types that belong to the `Fractional` class,
and we'll see that.

```haskell
-- Exercise: Experiment with Overloaded Literals in Haskell

-- 1. Define a function that takes a numeric input and returns its square.
square :: Num a => a -> a
square x = x * x

-- 2. Test the function with different numeric types.
main :: IO ()
main = do
    let intResult = square (5 :: Int)          -- Using Int
    let integerResult = square (5 :: Integer)  -- Using Integer
    let floatResult = square (5.0 :: Float)    -- Using Float
    let doubleResult = square (5.0 :: Double)  -- Using Double

    -- Print the results
    putStrLn $ "Square of 5 (Int): " ++ show intResult
    putStrLn $ "Square of 5 (Integer): " ++ show integerResult
    putStrLn $ "Square of 5.0 (Float): " ++ show floatResult
    putStrLn $ "Square of 5.0 (Double): " ++ show doubleResult

-- 3. Observe the output and note any differences in behavior or representation.
```

## The Num Class
Let's see what kind of classes play a role here, what methods they have, and
what kind of types play a role. I guess the most important class in the context
of numbers is the `Num` class. We see that the `Num` class has methods for
addition, subtraction, and multiplication. It also has `negate`, which is
another name for the unary minus, so `negate` computes the zero minus the number
that we are providing. Then we have a function that computes the absolute value,
and we have a function that depends on whether a number is an integer. This
function takes an integer into something of this type.

We have a minimal pragma, which is actually a little bit complicated. We have to
give at least `plus`, but not `minus`, `times`, or `abs`, but we do need to give
either `negate` or `minus`, one of the two. Conceptually, if we define `negate`,
then we can define `minus` as an addition with the negated second number. If we
define `minus`, then we have to give one of the two. In general, `Num` instances
are rarely defined for new types, but it is possible. One of the interesting
things in Haskell is that you can define your own number types or number-like
types.

## Instances of Num
Let's have a look at what instances we already have to work with. We have `Int`,
`Integer`, `Float`, `Double`, and `Word` available as a basis. Let's first talk
about `Int` and `Integer`, which I think I briefly mentioned before, but
probably not in very much detail.

## Int and Integer
`Int` is a bounded integer, so with overflow and whatever machines, in fact, I
think the Haskell standard is only saying that it must be bounded by a certain
number of bits. This means that, for example, `Int`s are also an instance of the
`Bounded` class, and we have a maximum value. We can add one to this and get an
overflow, and so on and so forth. These are our integers, whereas `Integer`,
written out, is not an instance of the `Bounded` class. If we ask for the
maximum bound of `Integer`, this is not an instance of the `Bounded` class. If
we take an `Int` and add one to it, and say we want to treat this whole thing as
an integer addition, it's perfectly fine, and I can get far larger numbers.

Another thing you can try is to compute the product of two numbers as an `Int`,
and you get a negative number indicating that something has gone wrong, and you
apparently have had an overflow somewhere. But even if you got a positive
number, it would not be clear that it is the correct result. If we compute this
as an `Integer`, we get a much larger result, but this is fine. `Integer` is
actually just bounded by the size of your memory. Small integers are still very
compactly represented, but not quite as compactly, of course, than a larger
number, which corresponds to more memory.

Computation with integers is a little bit slower than computation with `Int`,
but you never have to worry about overflow. Otherwise, `Int`s and `Integer`s
support pretty much the same operations, which is why there is another type
class that basically unifies these two. We will see that in a moment.

## Word
There is also `Word`. We can see here that `Word` is also bounded, but the
minimum bound of `Word` is just zero, not a negative number. The maximum bound
of `Word` is a correspondingly slightly larger number. If we add one to this on
`Word`s, we get an overflow on `Word`s, which starts from zero.

## Float and Double
Then we have `Float` and `Double`. These are single and double precision
floating-point numbers. In general, I think `Double` is used far more widely in
practice and in Haskell libraries than `Float` ever is. So I would generally not
recommend using `Float`.

Now, I think that's concluding the `Num` class and what we have to say about
this. Let's look at the `Integral` class next. The `Integral` class is for types
that are integral, such as `Int`, `Integer`, and `Word`, and all of these
support similar operations. There are a couple of operations that are being
listed here, and in particular, I think the most important operations are these
integer division operations such as `mod`, `div`, `rem`, and `quot`, which are
relatively similar. They behave slightly differently from each other if negative
numbers are involved, but as long as you're only using positive numbers, then
`div` and `quot` behave exactly the same, and `rem` and `mod` behave exactly the
same.

They're often used infix, so if you want to know the remainder, we can use `rem`
to get that remainder. If we want to do both at the same time, we can also use
this `divMod` combination, which gives us the two results as a pair. This works
for `Int`, `Integer`, and `Word` in the same way, only that, of course, for
`Int` and `Word`, you have to deal with the possibility of overflow. Note that
if you divide something by zero, you'll still get a crash, so this is a partial
function, which I'm not so harsh about because I think, in general, this case of
division by zero is a bit easier to rule out.

In general, you should still be very careful. You don't want to divide by
something that you cannot rule out to be zero, but often these kinds of
divisions are used with a concrete constant second argument anyway, and if that
is not zero, that is totally fine as an application.

## The Fractional Class
I think that's probably all we have to say about `Integral`, and then we have a
number of type classes that are used for various purposes. The first and most
interesting one, perhaps, is the `Fractional` class because it contains the
division operator. The division operator is as you would think, and then there
is this `recip` function, which basically computes one divided by a number.

The `Fractional` class includes `Float` and `Double`, but there is an
interesting type that is not exported via the Prelude, which is also an instance
of `Fractional`, and I do want to advertise that somewhat, and that is the type
of rational numbers. This is very interesting because, as I said, literals with
a dot are also overloaded, so they can be used as `Double`s and `Float`s, but
they can also be used as rational numbers.

## Rational Numbers
Rational numbers are internally a data type that is represented as a fraction of
two other numbers, typically integers. In particular, the `Rational` type is a
ratio of `Integer`, so you have two integers here, a fraction of two integers,
and that gives you a way to represent fractions precisely without any rounding
issues.

At the cost of not having certain functions available, particularly
trigonometric functions such as sine and cosine, or square roots and stuff like
that. There are certain functions that are not available, but as long as you
don't need them, you can avoid having any rounding issues by just going to
`Rational`, and it's certainly worth considering. In particular, all sorts of
financial calculations are typically much more nicely done using rational
numbers in my experience, or even using integers with a fixed precision, than by
using `Double`s and having to deal with potentially lost fractions of a currency
unit or something like that.

So, the `Rational` type is interesting. If we know that we have overloaded
literals, we can also do something like this, and indeed we see that if you do
something like a slightly more complicated number, it automatically reinterprets
this and applies optimization of the fractional representation as well. You can
calculate with rational numbers just as with other numbers, and in many
contexts, they're really just as useful.

## Conversion Functions
There is also this `fromRational` function, which is used for the fractional
literals in the same way as `fromInteger` is used for the integer literals. This
is going from rational because, in fact, whenever you have a literal with a dot
in it, it can be precisely represented as a rational number by dividing this
number that you have written here by the appropriate denominator to generate the
precise representation.

You can always convert precisely a literal fractional number into a rational
number, and then you can go from the rational number into the possibly more
lossy `Double` or `Float` representation. If we take something long and we say
we want to interpret this as a rational number, we get this. If we say we want
to interpret this as a `Double`, you can see that we get a somewhat lower
precision, and if we interpret this as a `Float`, we see that we get yet again a
somewhat lower precision.

I'm actually currently unsure whether the show instances are actually full
internal precision or whether there is more available, but nevertheless, this is
basically giving you an idea.

## Defaulting
Then there are some more classes that are giving you other commonly used
methods. There's this `Floating` class, which is implemented by `Float` and
`Double`, which gives you trigonometric functions and exponential functions. You
get exponential, logarithm, square root, pi, sine, cosine—all the things that
you would probably expect.

There are things such as the `RealFrac` class that gives you rounding functions.
In general, one thing that you probably do want to do occasionally is to either
convert from an integer into a floating-point number or the other way around. In
order to convert from a floating-point number back into an integer, you have to
make a decision. You would typically use something like `floor` or `ceiling` to
specifically generate the next smaller or the next larger integer number.

That is a way to convert a `Double` or a `Rational` into an integer if you need
this. You can see that it goes from something that is in the `RealFrac` class,
which means in particular it goes from a `Rational`, a `Float`, or a `Double` to
something that is in the `Integral` class, so that could be `Int`, `Integer`, or
`Word`.

There is also the `round` function. The `round` function, depending on different
countries, has slightly different cultures with rounding. The definition of the
`round` function may be a little bit unusual. In principle, it is closer to
three than to four, but what does it do with three or four? The `round` function
takes the perhaps slightly unusual approach to round to the nearest even number.

It doesn't always round up or always round down, but it rounds towards the
nearest even number. So if you have a number that is still four, it will round
to four. The idea is that it is overall a little bit more balanced than just
rounding up or rounding down consistently.

The other direction of conversion, if you want to go from an integer to a
floating-point number, is usually done using this `fromIntegral` function.
`fromIntegral` takes an integral as a source and has an arbitrary numeric type,
in particular, it can have a `Double` type as the target or a `Rational` type.
You can use `fromIntegral` to explicitly convert from an integer to a `Double`
if you need that.

Because of the nature of the overloaded literals, very often you don't actually
have to apply explicit conversion functions. If we say something automatically
makes it clear that we are in a fractional context, so even though this is
potentially an integer literal, the presence of this literal makes it clear that
we are suddenly now not in an integer context.

So we don't actually have to provide an explicit conversion, but sometimes in
combination with other functions, we still do. For example, if we have the
integer division of five and two, then we also get two out, but the type of five
divided by two is restricted to be a part of the `Integral` class because we
have already been applying the division function, which restricts its argument
to the `Integral` class.

So the type of five divided by two is more limited than the type of just writing
two. Here we are having something that is restricted to the `Integral` class,
and here we have a fractional literal. So what number type are we actually
supposed to use? The world of integral types and the world of fractional types
is disjoint.

At this point, we probably really want to explicitly convert this part here from
an integral into a fractional type, and we can do it like this. We don't even
need the parentheses here because it's a function application that binds
stronger than the operator. Sometimes you do need these conversion functions if
you combine functions in such a way that their result types are suddenly
becoming too specific, and the overloading is not doing the resolution for you
appropriately anymore.

There is never any implicit coercion going on in Haskell. The only kind of
things that feel a bit like coercion are to do with overloading. That is,
something that is flexible is still flexible between multiple types, but that
means that its type has never been completely determined yet. You can still make
a choice. If something is at some point of one concrete type, `Int` or `Double`
or something, that remains at that type. If you want to change it into something
else again, you have to apply an explicit coercion function such as
`fromIntegral` or something like that in the other direction.

Now, there's one more thing I want to mention, and that is something I've been
glossing over so far: that in the context of number types, we also very often
see defaulting being applied. Defaulting in the context of number types is
applied not just in GHC but even by Haskell, and it is somewhat confusing.

Again, if you enable warnings, if you enable all warnings, which is generally a
good idea, you see that I'll suddenly get all sorts of warnings. Why is this?
Well, because even if I just type the literal five now, which we've always been
glossing over, it's actually an overloaded value. It is the number five, but it
actually has the type `Num p => p`. So what instance do we mean? What is the
instance that we mean? We have to make a choice because we have not provided
contextual information.

If I explicitly say I want this to be an `Int` or if I explicitly say I want
this to be an `Integer`, then that's fine. But now if I type a very large
number, which of the two does it actually use? Does it use `Int` or `Integer`?
It may make a difference for overflows. By enabling the warning, we see that it
defaults, when it can, to `Integer`, not to `Int`.

So the preferred choice for `Num` class overloaded things, if a choice has to be
made and the contextual information is not explicitly present, is `Integer`.
That is what GHC does. The preferred choice for something of an overloaded type
is `Double`. We can again see this from the warning. If we don't want this, we
have to either provide an explicit type signature, such as `Rational`, or we
have to put it in a context that makes it clear.

I think that is probably a useful introduction that covers the basic types that
you want to deal with in Haskell. It really depends on a case-by-case analysis.
`Int`s are much more efficient, but if there is any concern about `Int`s
becoming large, you may want to use `Integer` and not worry about overflows.

`Double` is probably better than `Float`, but you should also always consider
that there is `Rational` available. If you don't want to worry about rounding
errors, it's a very attractive choice. Again, rationals cannot be as compactly
represented as `Double`s, but very often that is not a big deal.

Another option, if you want to avoid rounding errors, is just to use fixed
precision number types. You can either use `Integer` with an agreed precision,
or there are actually other libraries in Haskell as well for fixed numbers. What
we said about conversion is important: from `Integral` to go from integers
explicitly into the realm of fractional numbers, and functions such as `floor`
or `ceiling` or `round` to go from fractional numbers back to integers.

Defaulting actually plays quite a role. It is very often used, and it's usually
a better idea. That's also why it's a warning to be explicit, to provide the
extra type information in such situations and get rid of the warning by adding
either an extra type signature somewhere or an extra type annotation on some
sub-expression to make it clear and obvious which of the many numeric types you
mean.

Because literals are overloaded and many of the operators involved are so
flexible, if defaulting wouldn't take place, many expressions would actually
need extra type signatures, even just if you type them into GHCi. That's why
defaulting is there and is using `Integer` preferably or `Double` preferably for
integral or fractional numbers, respectively, to essentially provide a little
bit of convenience.
