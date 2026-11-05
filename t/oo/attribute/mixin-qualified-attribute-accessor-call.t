use Test;
plan 8;

# `self.R::output` inside a method of a role mixed into an R object names the
# class's own public attribute accessor, not the mixin's overriding `output`.
# It used to re-enter the mixin's method until the stack ran out.
class R { has $.output = "foo"; has $.n is rw = 1; method hello { "hello" } }
class S is R { has $.extra = "x" }

{
    my $d = R.new but role { method output { "d:" ~ self.R::output } };
    is $d.output, 'd:foo', 'a qualified accessor call reads the class attribute';
}
{
    my $d = R.new but role { method n { self.R::n + 10 } };
    is $d.n, 11, 'an `is rw` attribute accessor, qualified';
}
{
    my $d = R.new but role { method hello { "H:" ~ self.R::hello } };
    is $d.hello, 'H:hello', 'a plain method, qualified (unchanged)';
}
{
    my $e = S.new(output => "sub") but role { method output { "e:" ~ self.R::output } };
    is $e.output, 'e:sub', 'the accessor of an ancestor class, through a subclass instance';
}
{
    my $e = S.new but role { method extra { "E:" ~ self.S::extra } };
    is $e.extra, 'E:x', "the subclass's own accessor";
}
{
    my $f = R.new but role { method output { "f:" ~ self.R::output } };
    my $g = $f but role { method hello { "g:" ~ self.R::hello } };
    is $g.output, 'f:foo', 'a second mixin keeps the first one working';
    is $g.hello, 'g:hello', 'and the second works too';
}
{
    my $r = R.new(output => "own");
    my $d = $r but role { method output { "d:" ~ self.R::output } };
    is $d.output, 'd:own', 'the attribute value is the instance\'s, not the default';
}
