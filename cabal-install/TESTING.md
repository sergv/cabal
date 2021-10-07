# Testing `cabal-install`

Local testing
=======

In order to effectively test the `cabal-install` library, the `cabal-install.cabal` file must be modified
to build the targets in the `/test` directory. The current recommended way to set this up is to
use the [makefile](../Makefile) supplied in the `Cabal` project parent directory, issuing the following command:


```
> make cabal-install-dev
```

This command will copy the dev `.cabal` generated by a project build into the `cabal-install.cabal`, and set your git index to ignore
any changes to that file. Any subsequent changes to the `.cabal` should unset and reset the git index to make sure you don't end up committing it.
From there, tests may be built with `cabal test` as usual. To choose a particular test so you don't end up running the whole thing, you can issue
`tasty`-style pattern expressions like the following:

```
> cabal run cabal-install:unit-tests -- -p /cabal init/
```

Please remember to test your changes! Happy hacking.