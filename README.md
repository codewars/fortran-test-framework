## Fortran Test Framework

Codewars test framework for Fortran contributed by [@DonaldKellett](https://github.com/DonaldKellett).

### Spec Subroutines

```fortran
call describe(msg)
```

Begins a `describe` context using the message `msg` provided.  A `describe` context is useful in grouping all of the assertions used to test a particular function or module.  After all related assertions are executed, it is best practice to `call endContext()` to properly close the current `describe` context in order to avoid unintentional `describe` nesting.

```fortran
call it(msg)
```

Begins an `it` context using the message `msg` provided.  An `it` context is useful in grouping closely related assertions which test for the same feature/aspect of a given function or module.  `it` contexts should generally reside within `describe` contexts.  After the related group of assertions are executed, it is best practice to `call endContext()` to properly close the current `it` context in order to avoid unintentional `it` (and `describe`) nesting.

```fortran
call endContext()
```

A subroutine which does not accept any arguments and simply ends the most recently started context.  It should *never* be invoked *before* a `describe` and/or `it` context.

### Assertion Subroutines

```fortran
call assertEquals(expected, actual)
```

Directly compares two values for equality.  A number of commonly used data types are supported, including:

- `integer` (the default integer type)
- `integer(kind=8)`
- `integer(kind=16)`
- `logical` (default type only)
- `character` (default type only, supports both single characters and strings)

Note that: (1) both `expected` and `actual` must be of the same type when performing the assertion and (2) `assertEquals` for strings does more than simple equality comparison `==` - it also checks for the lengths of both strings to prevent false positives where a shorter string is padded to the right with whitespace characters and then considered equal with the longer string containing trailing whitespace characters.

```fortran
call assertEquals(expected, actual, msg)
```

Directly compares two values for equality, with the custom message `msg` (a string) specified upon failure.  The data types supported are identical to `assertEquals`, i.e. `integer`, `integer(kind=8)`, `integer(kind=16)`, `logical` and `character`.

```fortran
call assertNotEquals(unexpected, actual)
```

The inverse assertion of `assertEquals`.  The data types supported are identical to `assertEquals`.

```fortran
call assertNotEquals(unexpected, actual, msg)
```

The inverse assertion of `assertEquals` with the custom message `msg` (a string) displayed upon failure.  The data types supported are identical to `assertEquals`.

```fortran
call assertWithinTolerance(expected, actual, epsilon)
```

Compares two values for approximate equality within an absolute tolerance value `epsilon`.  This assertion method supports the following data types:

- `real` (default, single-precision floating point values)
- `real(kind=8)`
- `complex` (default type only)

The data type for `epsilon` varies depending on what `expected` and `actual` are - for `real` and `complex`, `epsilon` should be `real` while for `real(kind=8)` (double-precision values), `epsilon` should also be a double-precision value.

```fortran
call assertWithinTolerance(expected, actual, epsilon, msg)
```

Same as above except a custom message `msg` (a string) is shown upon failure.

```fortran
call assertNotWithinTolerance(unexpected, actual, epsilon)
```

Inverse assertion of `assertWithinTolerance`

```fortran
call assertNotWithinTolerance(unexpected, actual, epsilon, msg)
```

Inverse assertion of `assertWithinTolerance` with custom message `msg` displayed upon failure.


## Acknowledgements

Fortran test framework for Codewars was contributed by [@DonaldKellett](https://github.com/DonaldKellett) ([@donaldsebleung](https://www.codewars.com/users/donaldsebleung) on Codewars).
The code and the documentation was copied from [DonaldKellett/fortran-kata](https://github.com/DonaldKellett/fortran-kata) which permits Codewars to use without restrictions.
