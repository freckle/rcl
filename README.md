# Stackage Resolver Changelog

Fetch package differences between two Stackage resolvers, filter to dependencies
specified, and format relevant Changelog content for those upgrades.

## Installation

```console
git clone https://github.com/freckle/rcl
cd rcl
stack setup
stack install
~/.local/bin/rcl --help
```

## Usage

```
Usage: rcl [-d|--debug] (-f|--from RESOLVER) (-t|--to RESOLVER) [PACKAGE]
  Stackage resolver changelog

Available options:
  -h,--help                Show this help text
```

## Example

See `tests/integration` for actual arguments.

```console
rcl --from lts-14.21 --to lts-14.24 ...
```

Produces:

[Stackage diff](https://www.stackage.org/diff/lts-14.21/lts-14.24)

Changelogs:

## bifunctors

Changed from 5.5.6 to 5.5.7

> ---
>
> - Add `Data.Bifunctor.Biap`.

## shake

Changed from 0.18.4 to 0.18.5

> 0.18.5, released 2020-02-02 Use uninterruptibleMask\_ to ensure all cleanup
> happens robustly #742, make the Chrome profile only include the last run #740,
> make CmdArgument an instance of IsCmdArgument

## shakespeare

Changed from 2.0.23 to 2.0.24

> ### 2.0.24
>
> - Fix build errors with GHC 8.10.1-alpha2
>   [#245](https://github.com/yesodweb/shakespeare/pull/245)

## vector

Changed from 0.12.0.3 to 0.12.1.2

> - Fix for lost function `Data.Vector.Generic.mkType`:
>   [#287](https://github.com/haskell/vector/issues/287)
>
> # Changes in version 0.12.1.1 (deprecated)
>
> - add semigrioups dep to test suite so CI actually runs again on GHC < 8
>
> # Changes in version 0.12.1.0 (deprecated)
>
> - Fix integer overflows in specializations of Bundle/Stream enumFromTo on
>   Integral types
> - Fix possibility of OutOfMemory with `take` and very large arguments.
> - Fix `slice` function causing segfault and not checking the bounds properly.
> - updated specialization rule for EnumFromTo on Float and Double to make sure
>   it always matches the version in GHC Base (which changed as of 8.6) Thanks
>   to Aleksey Khudyakov @Shimuuar for this fix.
> - fast rejection short circuiting in eqBy operations
> - the O2 test suite now has reasonable memory usage on every GHC version,
>   special thanks to Alexey Kuleshevich (@lehins).
> - The `Mutable` type family is now injective on GHC 8.0 or later.
> - Using empty `Storable` vectors no longer results in division-by-zero errors.
> - The `Data` instances for `Vector` types now have well defined
>   implementations for `toConstr`, `gunfold`, and `dataTypeOf`.
> - New function: `partitionWith`.
> - Add `Unbox` instances for `Identity`, `Const`, `Down`, `Dual`, `Sum`,
>   `Product`, `Min`, `Max`, `First`, `Last`, `WrappedMonoid`, `Arg`, `Any`,
>   `All`, `Alt`, and `Compose`.
> - Add `NFData1` instances for applicable `Vector` types.

## yesod

Changed from 1.6.0 to 1.6.0.1

> - Remove unnecessary deriving of Typeable

## yesod-auth

Changed from 1.6.8 to 1.6.8.1

> ## 1.6.8.1
>
> - Email: Fix typo in `defaultEmailLoginHandler` template
>   [#1605](https://github.com/yesodweb/yesod/pull/1605)
> - Remove unnecessary deriving of Typeable

## yesod-core

Changed from 1.6.17 to 1.6.17.2

> ## 1.6.17.2
>
> - Support template-haskell 2.16, build with GHC 8.10
>   [#1657](https://github.com/yesodweb/yesod/pull/1657)
>
> ## 1.6.17.1
>
> - Remove unnecessary deriving of Typeable

**/Example**

---

[LICENSE](./LICENSE) | [CHANGELOG](./CHANGELOG.md)
