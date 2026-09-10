use v6;
use Test;

# Binding `self` to an outer variable (`$outer := self`) rewrites the frame's
# `self` entry into the bind's shared ContainerRef cell. The attribute WRITE
# paths (the `$!x = ...` type-object guard and the shared-cell resolution in
# `self_instance_attrs`) matched only Instance/Mixin, so after such a bind a
# scalar attribute assignment died with "Cannot look up attributes in a C type
# object" and an array attribute `.push` was silently lost from the instance
# (reads kept working — they resolve through the local slot). All expectations
# verified against raku.

plan 7;

my $s1;
class ScalarWrite {
    has $.x;
    method m() { $s1 := self; $!x = 5; }
}
my $sw = ScalarWrite.new(x => 1);
lives-ok { $sw.m }, 'attribute assignment after `$outer := self` does not die';
is $sw.x, 5, 'the assignment reaches the instance';

my $s2;
class TweakPush {
    has @.items;
    submethod TWEAK() { $s2 := self; @!items.push("t"); }
}
is TweakPush.new.items.elems, 1, 'array attr .push after `:= self` in TWEAK reaches the instance';

my $s3;
class MethodPush {
    has @.items;
    method m() { $s3 := self; @!items.push("t"); }
}
my $mp = MethodPush.new;
$mp.m;
is $mp.items.elems, 1, 'array attr .push after `:= self` in a plain method reaches the instance';
is $s3.items.elems, 1, 'the bound alias sees the push too';

my $s4;
class Mixed {
    has $.a;
    has $.b;
    method m() { $s4 := self; $!a = "A"; $!b = "B"; }
}
my $mx = Mixed.new;
$mx.m;
is "{$mx.a}{$mx.b}", "AB", 'multiple attribute writes after the bind all land';

# With the constructed-object identity fix (BUILD/TWEAK run against the object
# `.new` returns), a `:=`-stored self alias tracks post-construction mutations.
my $s5;
class StoredAlias {
    has $.x is rw;
    submethod TWEAK() { $s5 := self; }
}
my $sa = StoredAlias.new(x => 1);
$sa.x = 9;
is $s5.x, 9, 'a `:=`-stored self alias from TWEAK sees post-construction mutations';

done-testing;
