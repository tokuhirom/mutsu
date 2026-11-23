# `dd` prefixes its lines the way rakudo's does

rakudo's `dd` names an argument only when the argument is a container, and
puts the container's declared type in front. An `@` or `%` variable is not
itself a container:

```raku
my $x = 5; dd $x;        # $x = 5
my @a = 1, 2; dd @a;     # [1, 2]
my Int $y; dd $y;        # Int $y = Int
my Str %h; dd %h;        # Str  = (my Str %)
```

mutsu had this the other way round, printing `5` and `Array @a = [1, 2]`
(#18985). `dd` now builds the prefix the way rakudo's does. A writable scalar
variable (`$x`, `$*D`, `$!a`) shows its name, and a declared type goes in
front. A parameter, a `:=`-bound value, `self` and a literal show only the
value. `dd` also writes through `$*ERR` as rakudo's `note` does, so a test can
capture it. An element, alias or `is copy` container is still not named like
rakudo's (#19046), and the value part still uses the static renderer (#19047).
