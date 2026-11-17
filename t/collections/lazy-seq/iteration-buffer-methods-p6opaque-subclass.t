use Test;

# #17930: a class declared `is IterationBuffer` without `is repr('VMArray')`
# is P6opaque. It inherits IterationBuffer's methods, but each is an nqp op on
# `self` in rakudo, which the REPR refuses. Messages measured on rakudo.

plan 13;

my class SubBuf is IterationBuffer { }

sub refused(&code, $what, $desc) {
    throws-like &code, Exception,
        message => "This type (SubBuf) does not support $what", $desc;
}

refused { SubBuf.new.elems }, 'elems', '.elems is refused';
refused { SubBuf.new.AT-POS(0) }, 'positional operations', '.AT-POS is refused';
refused { SubBuf.new.BIND-POS(0, 1) }, 'positional operations', '.BIND-POS is refused';
refused { SubBuf.new.push(1) }, 'positional operations', '.push is refused';
refused { SubBuf.new.unshift(1) }, 'positional operations', '.unshift is refused';
refused { SubBuf.new.append(IterationBuffer.new) }, 'elems', '.append is refused';
refused { SubBuf.new.prepend(IterationBuffer.new) }, 'positional operations', '.prepend is refused';
refused { SubBuf.new.clear }, 'positional operations', '.clear is refused';
refused { SubBuf.new.List.gist }, 'positional operations', '.List is refused when read';
refused { SubBuf.new.Slip.gist }, 'positional operations', '.Slip is refused when read';
refused { SubBuf.new.Seq }, 'elems', '.Seq is refused';

# With the VMArray REPR the subclass keeps the storage.
my class VBuf is IterationBuffer is repr('VMArray') { }
my $v := VBuf.new;
$v.push(1);
$v.push(2);
is $v.elems, 2, 'a VMArray subclass answers .push/.elems';
is $v.List, (1, 2), 'and .List';
