use Test;

plan 29;

# The Buf mutators (ADR-11276 §8.3): rows of Buf, written through the binding.
my $b = Buf.new(1, 2, 3);
is $b.push(4).raku, 'Buf.new(1,2,3,4)', 'push answers the buffer';
$b.append(5, 6);
is $b.raku, 'Buf.new(1,2,3,4,5,6)', 'append of several';
$b.append(Buf.new(7));
is $b.raku, 'Buf.new(1,2,3,4,5,6,7)', 'append of a Buf';
$b.unshift(0);
is $b.raku, 'Buf.new(0,1,2,3,4,5,6,7)', 'unshift';
$b.prepend(9);
is $b.raku, 'Buf.new(9,0,1,2,3,4,5,6,7)', 'prepend';
is $b.pop, 7, 'pop answers the last element';
is $b.shift, 9, 'shift answers the first element';
is $b.raku, 'Buf.new(0,1,2,3,4,5,6)', 'pop and shift removed them';
is $b.splice(1, 2).raku, 'Buf.new(1,2)', 'splice answers the removed elements';
is $b.raku, 'Buf.new(0,3,4,5,6)', 'splice removed them';
is $b.splice(1, 1, 9).raku, 'Buf.new(3)', 'splice with a replacement';
is $b.raku, 'Buf.new(0,9,4,5,6)', 'the replacement is in';
$b.reallocate(2);
is $b.raku, 'Buf.new(0,9)', 'reallocate shrinks';
$b.reallocate(4);
is $b.raku, 'Buf.new(0,9,0,0)', 'reallocate grows with zeros';

my $e = Buf.new;
throws-like { $e.pop }, X::Cannot::Empty, 'pop of an empty Buf';
throws-like { $e.shift }, X::Cannot::Empty, 'shift of an empty Buf';
throws-like { $b.push("x") }, X::TypeCheck, 'a Str element is a type error';
is $b.elems, 4, 'a refused push left the buffer alone';

# Aliases share the buffer.
my $c = $b;
$c.push(100);
is $b.raku, 'Buf.new(0,9,0,0,100)', 'an alias sees the push';
sub grow(Buf $x) { $x.push(55) }
grow($b);
is $b.raku, 'Buf.new(0,9,0,0,100,55)', 'a callee sees the push';

class Holder {
    has Buf $.buf .= new;
    method add($v) { $!buf.push($v); self }
}
is Holder.new.add(1).add(2).buf.raku, 'Buf.new(1,2)', 'push on an attribute';

my buf8 $q .= new(5, 6);
$q.push(7);
is $q.raku, 'Buf[uint8].new(5,6,7)', 'push on a buf8';
my buf16 $w .= new(5, 6);
$w.push(300);
is $w.raku, 'Buf[uint16].new(5,6,300)', 'push on a buf16';

# A receiver with no binding: the copy is the answer.
is Buf.new(1, 2).append(3, 4).raku, 'Buf.new(1,2,3,4)', 'append on a temporary';
is Buf.new(1, 2, 3).pop, 3, 'pop on a temporary';
is Buf.new(1, 2, 3).shift, 1, 'shift on a temporary';
is Buf.new(1, 2).prepend(0).raku, 'Buf.new(0,1,2)', 'prepend on a temporary';

# A Blob is immutable.
throws-like { Blob.new(1, 2).push(3) }, Exception, 'Blob.push dies';
throws-like { my $bl = Blob.new(1); $bl.pop }, Exception, 'Blob.pop dies';

done-testing;
