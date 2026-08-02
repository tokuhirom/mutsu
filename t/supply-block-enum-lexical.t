use Test;

plan 8;

# A `my enum` binds its type name AND every variant name lexically in the block
# that declares it, so those names must (a) win over a same-named outer symbol
# inside that block and everything nested in it, and (b) not survive the block.

# --- (a) inside a `supply { }` body and its `whenever` callbacks.
#
# The callback is dispatched later from the emitting thread, whose ambient env
# is the main script's -- an outer binding that merely shares a variant's name
# must not win there. This is the shape Cro's request/response parsers use:
# `supply { my enum Expecting <StatusLine Header Body>; whenever ... }` next to
# a `Cro::HTTP::Header` class.

class SBEL-Header { }

my $src = Supplier.new;
my $out = supply {
    my enum SBEL-State <Start SBEL-Header Done>;
    is SBEL-Header.WHAT.^name, 'SBEL-State',
        'the variant wins over the file-scope class in the supply body';
    whenever $src.Supply -> $n {
        emit SBEL-Header.WHAT.^name;
        emit SBEL-Header.key;
        emit Done.value;
    }
};

my @got;
react {
    whenever $out -> $v { @got.push($v); done if @got == 3 }
    whenever Promise.in(5) { done }
    whenever Promise.in(0.2) { $src.emit(1) }
}

is @got[0], 'SBEL-State', 'and inside the whenever callback too';
is @got[1], 'SBEL-Header', 'the variant is the enum value, not the class';
is @got[2], 2, 'a later variant keeps its ordinal';

# --- (b) the binding dies with its block.

class SBEL-Outer { }

{
    my enum SBEL-Block <SBEL-Outer Q>;
    is SBEL-Outer.WHAT.^name, 'SBEL-Block', 'the variant wins inside the block';
}
is SBEL-Outer.^name, 'SBEL-Outer', 'and the outer class is intact after the block';

class SBEL-Sub { }
sub sbel-f() { my enum SBEL-InSub <SBEL-Sub Q2>; SBEL-Sub.WHAT.^name }
is sbel-f(), 'SBEL-InSub', 'the variant wins inside the sub body';
is SBEL-Sub.^name, 'SBEL-Sub', 'and the outer class is intact after the call';

done-testing;
