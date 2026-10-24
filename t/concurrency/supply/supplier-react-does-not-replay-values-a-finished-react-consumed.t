use v6;
use Test;

plan 6;

# A live Supplier does not hand a new `react` the values an earlier, finished
# `react` over the same Supply already consumed.
{
    my $sup = Supplier.new;
    my $sy = $sup.Supply;
    my @first;
    react { whenever $sy -> $v { @first.push($v); done if @first.elems >= 2 }; $sup.emit(10); $sup.emit(20); }
    my @second;
    react { whenever $sy -> $v { @second.push($v); done }; $sup.emit(30); }
    is-deeply @first, [10, 20], "the first react got its two values";
    is-deeply @second, [30], "the second react got only what was emitted after it started";
}

{
    my $sup = Supplier.new;
    my $sy = $sup.Supply;
    my @a;
    react { whenever $sy -> $v { @a.push($v); done }; $sup.emit(5); }
    my @b;
    react { whenever $sy -> $v { @b.push($v); done }; $sup.emit(7); }
    is-deeply @a, [5], "first react: the value emitted inside it";
    is-deeply @b, [7], "second react: not the 5 the first one consumed";
}

# The pattern of a singleton Supplier shared by sequential `start react`s.
class Hub {
    has Supplier $!supplier .= new;
    has Supply   $.events = $!supplier.Supply;
    method instance(::?CLASS:U: --> ::?CLASS:D) { $ //= self.bless }
    method new { !!! }
    method emit($x) { $!supplier.emit: $x }
}
{
    my @got1;
    my $p = start react whenever Hub.instance.events -> $e { @got1.push($e); done if @got1.elems >= 2 };
    sleep 0.2;
    Hub.instance.emit(1);
    Hub.instance.emit(2);
    await $p;
    my @got2;
    my $q = start react whenever Hub.instance.events -> $e { @got2.push($e); done };
    sleep 0.2;
    Hub.instance.emit(3);
    await $q;
    is-deeply @got1, [1, 2], "first start react";
    is-deeply @got2, [3], "second start react over the same singleton Supply";
}
