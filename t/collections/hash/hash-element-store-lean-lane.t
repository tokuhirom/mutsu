use Test;

# `%h{$k} = $v` into a declared `%` local takes the lean lane (#19285), with a
# `Str` or `Int` key (#19289: an overwrite builds no key String). These pin the
# shapes it must decline or get right: typed, defaulted, object and immutable
# hashes, `:=`-bound and rebound hashes, bound entries, aliasing through
# captures and threads, and the store's result value.

plan 24;

{
    my %h;
    %h<a> = 1;
    %h{'b'} = 2;
    is-deeply %h, {a => 1, b => 2}, 'Str-key inserts';
    %h<a> = 10;
    is %h<a>, 10, 'Str-key overwrite';
    is (%h<c> = 3), 3, 'the store evaluates to the stored value';
}

{
    my %h;
    %h{$_} = $_ * 2 for ^4;
    is-deeply %h, {'0' => 0, '1' => 2, '2' => 4, '3' => 6}, 'Int-key inserts file under the decimal Str';
    loop (my $j = 0; $j < 100; $j++) { %h{$j +& 3} = $j }
    is-deeply %h, {'0' => 96, '1' => 97, '2' => 98, '3' => 99}, 'Int-key overwrites in a loop';
    %h{-5} = 'neg';
    is %h<-5>, 'neg', 'a negative Int key';
    is %h{'3'}, 99, 'the Str spelling reads an Int-key store';
}

{
    my Int %t;
    %t<a> = 7;
    is %t<a>, 7, 'a typed hash takes a fitting value';
    throws-like { %t<b> = 'x' }, X::TypeCheck::Assignment, 'a typed hash rejects a wrong value';
}

{
    my %d is default(42);
    %d<a> = 1;
    is %d<a>, 1, 'an `is default` hash takes a plain store';
    %d<a> = Nil;
    is %d<a>, 42, 'a Nil store into it reads the default';
}

{
    my %o{Any};
    %o{1} = 'int';
    %o{'1'} = 'str';
    is %o{1}, 'int', 'an object hash keeps the Int key';
    is %o{'1'}, 'str', 'and the Str key apart';
}

{
    my %m := Map.new((a => 1));
    throws-like { %m<a> = 2 }, Exception, message => /immutable/, 'a bound Map rejects a store';
}

{
    my %backing = a => 1;
    my %alias := %backing;
    %alias<b> = 2;
    is %backing<b>, 2, 'a store through a `:=` alias reaches the original';
    %backing<c> = 3;
    is %alias<c>, 3, 'and the other way round';
}

{
    my %a = x => 1;
    my %b = x => 10;
    %a := %b;
    %a<x> = 99;
    is %b<x>, 99, 'a store into a rebound hash reaches its new container';
}

{
    my %h = k => 0;
    my $x = 0;
    %h<k> := $x;
    %h<k> = 'bound';
    is $x, 'bound', 'a store into a bound entry writes through to the variable';
}

{
    my %h = k => 1;
    my $r = %h;
    %h<k> = 'seen';
    is $r<k>, 'seen', 'an itemized reference sees the store';
    my &f = { %h<c> = 'closure' };
    f();
    is %h<c>, 'closure', 'a closure store is seen by the declaring scope';
}

{
    my %h;
    await start { %h<w> = 'worker' };
    is %h<w>, 'worker', 'a worker store is seen by the main thread';
}

{
    my %h;
    %h<a> = [1, 2];
    is %h<a>.elems, 2, 'an array value is stored as one itemized element';
    %h<s> = (3, 4).Seq;
    is %h<s>.elems, 2, 'a Seq value is stored as one element';
}

{
    my %h;
    for <x y x> -> $k { %h{$k} = (%h{$k} // 0) + 1 }
    is-deeply %h, {x => 2, y => 1}, 'a counting idiom';
}
