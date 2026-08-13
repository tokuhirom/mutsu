Module [![Test raku](https://github.com/Tux/CSV/actions/workflows/test.yaml/badge.svg)](https://github.com/Tux/CSV/actions/workflows/test.yaml)
------
Text::CSV - Handle CSV data in Raku

Description
-----------
Text::CSV provides facilities for the composition and decomposition
of comma-separated values.  An instance of the Text::CSV class can
combine fields into a CSV string and parse a CSV string into fields.

This module provides both an OO API and a functional API to parse
and produce CSV data.
```
 use Text::CSV;

 my $csv = Text::CSV.new;
 my $io  = open "file.csv", :r, chomp => False;
 my @dta = $csv.getline_all($io);

 my @dta = csv(in => "file.csv");
```

Additional (still incomplete) documentation in [the `doc` directory](/doc), including [a markdown version](/doc/Text-CSV.md). Check out also the [examples](/examples).

Debugging information can be obtained by setting the `RAKU_VERBOSE`
environment variable with values ranging to 2 to 9, less to annoyingly verbose.

## Installation 

Recent changes can be (re)viewed in the public GIT repository at
https://github.com/Tux/CSV
Feel free to clone your own copy:
```
 $ git clone https://github.com/Tux/CSV Text-CSV
```

Prerequisites
-------------
* raku 6.c
* File::Temp   - as long as in-memory IO is not native
* Slang::Tuxic - to support my style

Build/Installation
------------------
```
 $ zef install Text::CSV
``` 

Or just 

```shell
$ zef install .
```

for the already downloaded repo
 
License
-------
Copyright (c) 2015-2023 H.Merijn Brand.  All rights reserved.

This program is free software; you can redistribute it and/or
modify it under the same terms as Raku itself, which is
GNU General Public License or Artistic License 2.


Author
------
H.Merijn Brand <h.m.brand@xs4all.nl>
