use lib 't/lib';
use Test;
use CallerWritebackShadow;

# A block that writes a caller lexical records that write so the frame owning
# the variable refreshes its slot on return. The record was a bare name, so
# the first frame to drain with a local of that name took it -- including a
# frame entered LATER and deeper, here `describe`'s `$desc is copy`. The
# caller's own `$desc` then kept its old value. That is how
# `lives-ok { $desc = ... }, '...'` lost its write: Test's `proclaim` has a
# `$desc is copy` parameter.

plan 4;

{
    my $desc = 'before';
    runs-then-describes { $desc = 'after' }, :label<x>;
    is $desc, 'after', 'the block write survives a deeper same-named parameter';
}

{
    my $desc = 'before';
    runs-then-describes { $desc = 'after' };
    is $desc, 'after', 'same without a named argument';
}

{
    my $desc = 'before';
    lives-ok { $desc = 'after' }, 'writes a caller lexical named $desc';
    is $desc, 'after', 'the lives-ok block write reached the caller';
}
