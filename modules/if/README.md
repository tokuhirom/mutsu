[![Actions Status](https://github.com/raku-community-modules/if/actions/workflows/linux.yml/badge.svg)](https://github.com/raku-community-modules/if/actions) [![Actions Status](https://github.com/raku-community-modules/if/actions/workflows/macos.yml/badge.svg)](https://github.com/raku-community-modules/if/actions) [![Actions Status](https://github.com/raku-community-modules/if/actions/workflows/windows.yml/badge.svg)](https://github.com/raku-community-modules/if/actions)

NAME
====

if - Conditionally load a distribution

SYNOPSIS
========

```raku
use if; # activate the :if adverb on use statements

use My::Linux::Backend:if($*KERNEL.name eq 'linux');
use My::Fallback::Backend:if($*KERNEL.name ne 'linux');

# ... do something with the backend you got
```

DESCRIPTION
===========

The `if` distribution will let you conditionally load a distribution. Use cases (no pun intended) are about loading different implementations of a functionality for different operating systems, compiler backends, or compiler versions.

This means that these switches for different implementations do not happen at runtime, but cheaply at compile time. This also means that a custom build and install hook is not needed because all implementations are installed. Then depending on the conditions only the desired implementation will be used.

Even if the switch is by backends you can share one installation by several backends using this technique.

AUTHOR
======

Tobias Leich

COPYRIGHT AND LICENSE
=====================

Copyright 2016-2020 Tobias Leich

Copyright 2023, 2024, 2026 Raku Community

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

