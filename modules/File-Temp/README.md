[![Actions Status](https://github.com/raku-community-modules/File-Temp/actions/workflows/linux.yml/badge.svg)](https://github.com/raku-community-modules/File-Temp/actions) [![Actions Status](https://github.com/raku-community-modules/File-Temp/actions/workflows/macos.yml/badge.svg)](https://github.com/raku-community-modules/File-Temp/actions) [![Actions Status](https://github.com/raku-community-modules/File-Temp/actions/workflows/windows.yml/badge.svg)](https://github.com/raku-community-modules/File-Temp/actions)

NAME
====

File::Temp - Create temporary files & directories

SYNOPSIS
========

```raku
# Generate a temporary directory
my $tmpdir = tempdir;

# Generate a temporary file in a temporary dir
my ($filename, $filehandle) = tempfile;

# specify a template for the filename
#  * are replaced with random characters
my ($filename, $filehandle) = tempfile("******");

# Automatically unlink files at end of program (this is the default)
my ($filename, $filehandle) = tempfile("******", :unlink);

# Specify the directory where the tempfile will be created
my ($filename, $filehandle) = tempfile(:tempdir("/path/to/my/dir"));

# don't unlink this one
my ($filename, $filehandle) = tempfile(:tempdir('.'), :!unlink);

# specify a prefix and suffix for the filename
my ($filename, $filehandle) = tempfile(:prefix('foo'), :suffix(".txt"));
```

DESCRIPTION
===========



This module exports two routines:

  * tempfile Creates a temporary file and returns a filehandle to that file opened for writing and the filename of that temporary file

  * tempdir Creates a temporary directory and returns the directory name

AUTHORS
=======

  * Jonathan Scott Duff

  * Rod Taylor

  * Polgár Márton

  * Tom Browder

Source can be located at: https://github.com/raku-community-modules/File-Temp . Comments and Pull Requests are welcome.

COPYRIGHT AND LICENSE
=====================

Copyright 2012 - 2017 Jonathan Scott Duff

Copyright 2018 - 2021 Rod Taylor

Copyright 2022 - 2025 Raku Community

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

