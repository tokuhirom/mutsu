[![Actions Status](https://github.com/raku-community-modules/Slang-Tuxic/actions/workflows/linux.yml/badge.svg)](https://github.com/raku-community-modules/Slang-Tuxic/actions) [![Actions Status](https://github.com/raku-community-modules/Slang-Tuxic/actions/workflows/macos.yml/badge.svg)](https://github.com/raku-community-modules/Slang-Tuxic/actions) [![Actions Status](https://github.com/raku-community-modules/Slang-Tuxic/actions/workflows/windows.yml/badge.svg)](https://github.com/raku-community-modules/Slang-Tuxic/actions)

NAME
====

Slang::Tuxic - allow whitespace between subroutine and the opening parenthesis

SYNOPSIS
========

```raku
foo 3, 5;   # 15, as usual
foo(3, 5);  # also 15, as usual
foo (3, 5); # 15, /o\

# It also allows to put space before argument lists in method calls:
42.fmt('-%d-');  # -42-
42.fmt: '-%d-';  # -42-
42.fmt ('-%d-'); # -42-
```

DESCRIPTION
===========

The `Slang::Tuxic` module adapts the Raku Programming Language syntax to allow you to put whitespace between the name of a subroutine and the opening parenthesis when calling a subroutine.

Be aware that this introduces ambiguous situations, like when you want to pass a List to a sub, or when you need parenthesis around the condition after the keywords `if`, `while` and so on.

AUTHORS
=======

  * Tobias Leich (FROGGS)

Source can be located at: https://github.com/raku-community-modules/Slang-Tuxic . Comments and Pull Requests are welcome.

COPYRIGHT AND LICENSE
=====================

Copyright 2014 - 2018 Tobias Leich

Copyright 2023 - 2025 Raku Community

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

