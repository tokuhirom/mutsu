use v6.e.PREVIEW;
use Test;

plan 13;

# Formatter::Syntax.parse returns a Match of the format string (S32-str/format.t)
my $m = Formatter::Syntax.parse("zippo%10sbar");
isa-ok $m, Match, 'Formatter::Syntax.parse returns a Match';
is ~$m, "zippo%10sbar", 'the Match spans the whole format string';

# Formatter.CODE returns a Callable that renders the format
my $code = Formatter.CODE("%5s");
isa-ok $code, Callable, 'Formatter.CODE returns a Callable';
is $code("foo"), "  foo", 'the returned Callable renders the format';

my $code2 = Formatter.CODE("zippo%10sbar");
is $code2("x"), "zippo         xbar", 'Formatter.CODE renders literal + directive';

# Format.new callability parity with Formatter.CODE
my $f = Format.new("%5s");
is $f("bar"), $code("bar"), 'Format.new and Formatter.CODE agree';

# Formatter::Syntax.parse of a directive-free format still parses
isa-ok Formatter::Syntax.parse("plain text"), Match,
  'a literal-only format parses';

# arity/count reflected through Format
is Format.new("%5s:%5s").arity, 2, 'two-directive format has arity 2';
is Format.new("foo").count, 0, 'literal-only format has count 0';

# `.lazy.List`/`.lazy.Seq` keep the list lazy (S32-str/format.t 46/49): a
# subsequent `.fmt` must throw X::Cannot::Lazy rather than materializing.
my $list := <a a b b>;
ok $list.lazy.List.is-lazy, '.lazy.List stays lazy';
ok $list.lazy.Seq.is-lazy,  '.lazy.Seq stays lazy';
throws-like { $list.lazy.List.fmt($f) }, X::Cannot::Lazy,
  '.fmt on a lazy List throws X::Cannot::Lazy';
throws-like { $list.lazy.Seq.fmt($f) }, X::Cannot::Lazy,
  '.fmt on a lazy Seq throws X::Cannot::Lazy';
