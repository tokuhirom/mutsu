[![Actions Status](https://github.com/raku-community-modules/File-Directory-Tree/actions/workflows/linux.yml/badge.svg)](https://github.com/raku-community-modules/File-Directory-Tree/actions) [![Actions Status](https://github.com/raku-community-modules/File-Directory-Tree/actions/workflows/macos.yml/badge.svg)](https://github.com/raku-community-modules/File-Directory-Tree/actions) [![Actions Status](https://github.com/raku-community-modules/File-Directory-Tree/actions/workflows/windows.yml/badge.svg)](https://github.com/raku-community-modules/File-Directory-Tree/actions)

NAME
====

File::Directory::Tree - Create, delete and empty directories

SYNOPSIS
========

```raku
use File::Directory::Tree;

# make a new directory tree
mktree "foo/bar/baz/quux";

# delete what you just made
rmtree "foo";

# clean up your /tmp -- but don't delete /tmp itself
empty-directory "/tmp";
```

DESCRIPTION
===========

The `File::Directory::Tree` distributin provides recursive versions of `mkdir` and `rmdir`, as well as an `empty-directory` subroutine for emptying out directories.

SUBROUTINES
===========

mktree
------

```raku
# make a new directory tree
mktree "foo/bar/baz/quux";
```

Makes a directory tree with the given path (which can either be a string or an `IO::Path` object). If any elements of the tree do not already exist, this will make new directories.

Accepts an optional second argument, the permissions mask. This is supplied to `mkdir`, and used for all of the directories it makes; the default is `0o777` (`a+rwx`). It takes an integer, but you really should supply something in octal like `0o755` or `:8('500')`.

Returns `True` if successful.

rmtree
------

```raku
# delete everything below the directory "foo" and "foo" itself
rmtree "foo";
```

This deletes all files under a directory tree with the given path (which can either be a string or an `IO::Path` object) before deleting the directory itself. It will recursively call `unlink` and `rmdir` until everything under the path is deleted.

Returns True if successful, and False if it cannot delete a file or remove a directory.

empty-directory
---------------

```raku
# clean up your /tmp -- but don't delete /tmp itself
empty-directory "/tmp";
```

Deletes all items in a given directory (which can be specified either by a string or by an `IO::Path` object), but leaves the directory itself intact. After running this, the only thing in the directory should be '.' and '..'.

Returns True if successful, and False if it cannot delete a file or a directory.

AUTHORS
=======

  * Brent Laabs

  * Paul Cochrane

  * Zoffix Znet

Source can be located at: https://github.com/raku-community-modules/File-Directory-Tree . Comments and Pull Requests are welcome.

COPYRIGHT AND LICENSE
=====================

Copyright 2013 - 2024 Brent Laabs

Copyright 2025 Raku Community

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

