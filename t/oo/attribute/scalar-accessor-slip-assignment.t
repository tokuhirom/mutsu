use Test;

plan 5;

# CRDT 0.0.16's LWW-Register.copy assigns a slipped scalar through a private rw accessor.
class SlipHolder {
    has $!value;
    has $.public is rw;

    method !value is rw { $!value }
    method seed { $!value = 13; self }
    method read-private { $!value }

    method copy-private {
        my $copy = SlipHolder.new;
        $copy!value = |$!value;
        $copy
    }

    method copy-public {
        my $copy = SlipHolder.new;
        $copy.public = |$!value;
        $copy
    }
}

my $source = SlipHolder.new.seed;
my $private-copy = $source.copy-private;
my $public-copy = $source.copy-public;

is $private-copy.read-private.^name, 'Slip', 'private rw attribute assignment keeps the slipped value';
is $private-copy.read-private.raku, '$(slip(13,))', 'private rw attribute stores the Slip itself';
is $public-copy.public.^name, 'Slip', 'public rw attribute assignment keeps the slipped value';
is $public-copy.public.raku, '$(slip(13,))', 'public rw attribute stores the Slip itself';

sub forward-is-rw($slot is rw) is rw { return-rw $slot }
my $forwarded = 0;
forward-is-rw($forwarded) = slip(13);
is $forwarded.raku, '$(slip(13,))', 'an rw routine itemizes a Slip written through its scalar container';
