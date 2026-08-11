[![Actions Status](https://github.com/lizmat/Slangify/actions/workflows/linux.yml/badge.svg)](https://github.com/lizmat/Slangify/actions) [![Actions Status](https://github.com/lizmat/Slangify/actions/workflows/macos.yml/badge.svg)](https://github.com/lizmat/Slangify/actions) [![Actions Status](https://github.com/lizmat/Slangify/actions/workflows/windows.yml/badge.svg)](https://github.com/lizmat/Slangify/actions)

NAME
====

Slangify - Provide an easy interface to activating slangs

SYNOPSIS
========

```raku
# Code of a slang module with only a grammar change
role Foo::Grammar { ... }
use Slangify Foo::Grammar, Mu;

# Code of a slang module with both grammar and actions
role Foo::Grammar { ... }
role Foo::Actions { ... }
use Slangify Foo::Grammar, Foo::Actions;

# Code of a slang module with only an actions change
role Foo::Actions { ... }
use Slangify Mu, Foo::Actions;

# code of a slang module with legacy grammar
role Foo::Grammar { ... }
role Foo::Actions { ... }
role Foo::Grammar::Legacy { ... }
role Foo::Actions::Legacy { ... }
use Slangify Foo::Grammar, Foo::Actions,
  Foo::Grammar::Legacy, Foo::Actions::Legacy;
```

DESCRIPTION
===========

The `Slangify` module is a module intended for slang developers.

It abstracts the internals of slang creation and activation so that module developers of slangs have a consistent interface they can work with across current and future versions of the Raku Programming Language.

This module is **only** needed to install a module that provides a slang. And as such, would only need to be specified in the `build-depends` section of the META information.

Note that the absence of a grammar role or an actions role, can be indicated by specifying `Mu`. Sadly, some core ideosyncracies make it currently impossible to indicate no change otherwise.

If the given grammar / actions can work with both the legacy grammar, as well as with the new RakuAST based grammar, then no changes are needed. If there **is** a different grammar for the legacy grammar and/or actions for the legacy grammar, then you can specify these as the 3rd and 4th argument in the `use Slangify` statement.

ADDITIONAL EXPORTS
==================

The `Slangify` module also exports special candidates for postcircumfix `{ }` and for prefix `~`. This allows slang developers to remain closer to original grammar code which is often living in NQP land, e.g.: `~$<identifier> eq 'foo'`. Without these special candidates, slang developers would need to resort to using NQP ops.

AUTHOR
======

Elizabeth Mattijsen <liz@raku.rocks>

Source can be located at: https://github.com/lizmat/Slangify . Comments and Pull Requests are welcome.

If you like this module, or what I’m doing more generally, committing to a [small sponsorship](https://github.com/sponsors/lizmat/) would mean a great deal to me!

COPYRIGHT AND LICENSE
=====================

Copyright 2023, 2025 Elizabeth Mattijsen

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

