use Test;

# From CSS::Stylesheet's CSS::AtPageRule: `has CSS::Properties() %.margin-box`
# and `%!margin-box{$name} .= new: :$ast`. A missing element of a
# coercion-typed hash is the TARGET type object, so `.new` reaches the class.
plan 7;

class P {
    has $.ast;
    method new(:$ast) { self.bless(:$ast) }
    method COERCE($x) { P.new(ast => $x) }
}

my P() %h;
is %h<missing>.WHAT.^name, 'P', 'the missing element is the target type object';

my $k = 'a';
%h{$k} .= new: :ast('x');
is %h<a>.ast, 'x', '.= new on a missing element builds an instance';
ok %h<a>.defined, 'and stores it';

class R {
    has P() %.box;
    has P() $.single;
    submethod TWEAK() {
        $!single .= new: :ast('s');
        my $name = 'top';
        %!box{$name} .= new: :ast('b');
    }
}
my $r = R.new;
is $r.box<top>.ast, 'b', 'inside a submethod on an attribute hash';
is $r.single.ast, 's', 'the scalar attribute form';

my P() @a;
is @a[3].WHAT.^name, 'P', 'array elements default the same way';
@a[1] .= new: :ast('e');
is @a[1].ast, 'e', 'and .= new builds there too';

# vim: expandtab shiftwidth=4
