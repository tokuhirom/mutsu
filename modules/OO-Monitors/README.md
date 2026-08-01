[![Actions Status](https://github.com/raku-community-modules/OO-Monitors/actions/workflows/linux.yml/badge.svg)](https://github.com/raku-community-modules/OO-Monitors/actions) [![Actions Status](https://github.com/raku-community-modules/OO-Monitors/actions/workflows/macos.yml/badge.svg)](https://github.com/raku-community-modules/OO-Monitors/actions) [![Actions Status](https://github.com/raku-community-modules/OO-Monitors/actions/workflows/windows.yml/badge.svg)](https://github.com/raku-community-modules/OO-Monitors/actions)

NAME
====

OO::Monitors - Objects with mutual exclusion and condition variables

SYNOPSIS
========

```raku
use OO::Monitors;

monitor Foo {
    has $.bar

    # accessible by one thread at a time
    method frobnicate() { }
}
```

DESCRIPTION
===========

A monitor provides per-instance mutual exclusion for objects. This means that for a given object instance, only one thread can ever be inside its methods at a time. This is achieved by a lock being associated with each object. The lock is acquired automatically at the entry to each method in the monitor. Condition variables are also supported.

Basic Usage
-----------

A monitor looks like a normal class, but declared with the `monitor` keyword.

```raku
use OO::Monitors;

monitor IPFilter {
    has %!active;
    has %!blacklist;
    has $.limit = 10;
    has $.blocked = 0;

    method should-start-request($ip) {
        if %!blacklist{$ip}
          || (%!active{$ip} // 0) == $.limit {
            $!blocked++;
            return False;
        }
        else {
            %!active{$ip}++;
            return True;
        }
    }

    method end-request($ip) {
        %!active{$ip}--;
    }
}
```

That's about all there is to it. The monitor meta-object enforces mutual exclusion.

Circular waiting
----------------

Monitors are vulnerable to deadlock, if you set up a circular dependency. Keep object graphs involving monitors simple and cycle-free, so far as is possible.

Cloning
-------

You can clone a monitor. The cloned monitor will have its own lock to prevent further chances of deadlock.

AUTHOR
======

Jonathan Worthington

Source can be located at: https://github.com/raku-community-modules/OO-Monitors . Comments and Pull Requests are welcome.

COPYRIGHT AND LICENSE
=====================

Copyright 2014 - 2021 Jonathan Worthington

Copyright 2024, 2025 Raku Community

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

