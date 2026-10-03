use Test;

# `temp` on a method lvalue: `$.attr` (which is `self.attr`), `self.attr`
# and `$obj.attr`, with plain and compound assignment. The value is
# restored when the enclosing scope exits. From CSS::Writer's
# `temp $.indent ~= ' ' x $indent`.

plan 10;

class C {
    has $.indent is rw = '';
    has $.n is rw = 1;

    method nested-indent {
        temp $.indent ~= '  ';
        "[{$.indent}]"
    }
    method assign-dot {
        temp $.n = 42;
        $.n
    }
    method assign-self {
        temp self.n = 7;
        self.n
    }
    method compound-var {
        my $o = self;
        temp $o.indent ~= 'c';
        $o.indent
    }
}

my $c = C.new;
is $c.nested-indent, '[  ]', 'temp $.attr ~= value updates inside the scope';
is $c.indent, '', '... and is restored afterwards';

is $c.assign-dot, 42, 'temp $.attr = value';
is $c.n, 1, '... restored';

is $c.assign-self, 7, 'temp self.attr = value';
is $c.n, 1, '... restored';

$c.indent = 'ab';
is $c.compound-var, 'abc', 'temp $obj.attr ~= value';
is $c.indent, 'ab', '... restored';

{
    temp $c.n += 10;
    is $c.n, 11, 'temp $obj.attr += value at block scope';
}
is $c.n, 1, '... restored on block exit';
