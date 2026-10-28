use Test;

# A Date/DateTime subclass that overrides `Str` is stringified through it by
# interpolation, `.gist`, `.Stringy` and an Array's `.gist` (`Date.gist` and
# `Mu.Stringy` are `self.Str`), not only by `~`.

plan 12;

class P is Date { method Str { "custom" } }
my $p = P.new("2020-01-02");
is "$p", 'custom', 'interpolation';
is "a$p", 'acustom', 'interpolation inside a longer string';
is $p.gist, 'custom', '.gist';
is $p.Stringy, 'custom', '.Stringy';
my @a = $p;
is @a.gist, '[custom]', "an Array's .gist";
is ~$p, 'custom', 'prefix ~';

class Q is DateTime { method Str { "q" } }
my $q = Q.new("2020-01-02T03:04:05Z");
is "$q", 'q', 'DateTime subclass: interpolation';
is $q.gist, 'q', '.gist';
is $q.Stringy, 'q', '.Stringy';
is [$q].gist, '[q]', "an Array's .gist";

is Date.new("2020-01-02").gist, '2020-01-02', 'a plain Date is unchanged';
class R is Date { }
is R.new("2020-01-02").Stringy, '2020-01-02', 'a subclass without Str is unchanged';
