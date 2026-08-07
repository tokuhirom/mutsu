use Test;

plan 3;

# A `supply` block's own `my` lexicals are promoted to shared container cells so
# every callback the block registers reads and writes ONE binding. An rw-accessor
# assignment on the object such a lexical holds (`$obj.tag = ...`) used to write
# the mutated object back by *replacing* the binding, which dropped the shared
# cell: a nested `sub` reassigning the lexical then wrote a cell nobody read any
# more, so the next `whenever` invocation saw the previous object.
#
# This is why Cro's HTTP request parser could not handle pipelined requests: it
# holds `my $request`, reassigns it from `sub fresh-message`, and sets
# `$request.method` in the `whenever` body.

class Obj { has $.id; has $.tag is rw; my $c = 0; submethod TWEAK { $!id = ++$c } }

{
    my $src = Supplier.new;
    my @got;
    my $s = supply {
        my $obj;
        my sub fresh() { $obj = Obj.new }
        fresh;
        whenever $src -> $v {
            $obj.tag = "t$v";
            emit "$v:{$obj.id}:{$obj.tag}";
            fresh();
        }
    };
    $s.tap(-> $x { @got.push($x) });
    $src.emit(1);
    $src.emit(2);
    $src.emit(3);
    is @got, ["1:1:t1", "2:2:t2", "3:3:t3"],
        'a nested sub reassigning a supply-block lexical survives an rw-accessor write';
}

# The same for an rw accessor written from the nested sub and read in the body.
{
    my $src = Supplier.new;
    my @got;
    my $s = supply {
        my $obj = Obj.new;
        my sub stamp($v) { $obj.tag = "s$v" }
        whenever $src -> $v {
            stamp($v);
            emit $obj.tag;
        }
    };
    $s.tap(-> $x { @got.push($x) });
    $src.emit(1);
    $src.emit(2);
    is @got, ["s1", "s2"], 'an rw-accessor write from a nested sub is visible in the body';
}

# Sibling whenevers keep sharing the block lexical across an rw-accessor write.
{
    my $a = Supplier.new;
    my $b = Supplier.new;
    my @got;
    my $s = supply {
        my $obj = Obj.new;
        whenever $a -> $v { $obj.tag = "a$v" }
        whenever $b -> $v { emit $obj.tag }
    };
    $s.tap(-> $x { @got.push($x) });
    $a.emit(1);
    $b.emit(1);
    $a.emit(2);
    $b.emit(2);
    is @got, ["a1", "a2"], 'a sibling whenever sees the rw-accessor write';
}
