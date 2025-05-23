# Style Guide

## `fourmolu`

This repository uses
[`fourmolu`](https://hackage.haskell.org/package/fourmolu) as an
autoformatter.

## `editorconfig`

This repository has an `.editorconfig` file for use with the
[`EditorConfig`](https://editorconfig.org/) tool. It's recommended to install
the tool so that the editor style is picked up automatically.

## General Style Guide

Prefer 4 space indentation. If the line gets too long, refactor the code - pull
out named terms into `let` or `where` bindings (or top-level functions).

Prefer `case` expressions over combinators. Prefer `do` notation over combinators.
It's easier, simpler, and faster to read and modify these forms than more
concise versions, even where the more concise version is faster to write at
first.

# Building with Backends

With all required backends installed, `stack build` can build all packages
listed in `stack.yaml` and is equivalent to:

```
> stack build persistent persistent-template persistent-sqlite persistent-test
persistent-mongoDB persistent-mysql persistent-postgresql persistent-redis
```

If backends such as mysql and postgres are not installed then the default build
will fail as will builds for packages for those backends alone:

```
> stack build persistent-mysql
...
    Process exited with code: ExitFailure 1
    Configuring mysql-0.1.4...
    setup: The program 'mysql_config' is required but it could not be found

> stack build persistent-postgresql
...
    Process exited with code: ExitFailure 1
    Configuring postgresql-libpq-0.9.4.0...
    setup: The program 'pg_config' is required but it could not be found.
```

To build all other packages, drop the failing package names as targets:

```
> stack build persistent persistent-template persistent-sqlite persistent-test
persistent-mongoDB persistent-redis
...
Completed 6 action(s).
```

# Running persistent tests using Stack

To run all the tests for the repository, run:

    > stack test

For testing specific packages, you can run:

    > stack test persistent-sqlite

This will run the tests for the `persistent-sqlite` package alone.

# Running persistent tests using Cabal

    > cabal new-test all

To test a specific package, you'll pass the package names instead of `all`.

# Testing Backends

The different backend libraries (`persistent-postgresql`, `persistent-mysql`, etc) are tested in their respective package directories.
`persistent-sqlite` requires 0 additional setup.
The other packages require some amount of setup in order to run.
Details for setup in these should be present in those package directories.
