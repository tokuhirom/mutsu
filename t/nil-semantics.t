use Test;

# Subscripting Nil (positionally or associatively) yields Nil again, so
# chained access keeps returning Nil rather than an out-of-range Failure.
{
    sub niltest { Nil }
    ok niltest[0]          === Nil, 'Nil[0] is Nil';
    ok niltest[0][2]       === Nil, 'Nil[0][2] is Nil (chained, index > 0)';
    ok niltest[0][2][4]    === Nil, 'Nil[0][2][4] is Nil';
    ok niltest<foo><bar>   === Nil, 'Nil<foo><bar> is Nil';
    ok niltest.foo.bar.<bar>.[12].[99].<foo> === Nil, 'mixed .<>/.[] chain is Nil';
}

# `Nil.push` / `.append` / `.unshift` / `.prepend` on a *literal* Nil throws
# (autovivification only applies to container elements / variables).
{
    throws-like { Nil.push },    Exception, 'Nil.push throws';
    throws-like { Nil.append },  Exception, 'Nil.append throws';
    throws-like { Nil.unshift }, Exception, 'Nil.unshift throws';
    throws-like { Nil.prepend }, Exception, 'Nil.prepend throws';
}

# Autovivification of an undefined container element / variable still works.
{
    my @a;
    @a[5].push(2);
    is-deeply @a[5], [2], '@a[5].push autovivifies an Array';
    my %h;
    %h<k>.push(1);
    is-deeply %h<k>, [1], '%h<k>.push autovivifies an Array';
    my $x;
    $x.push(3);
    is-deeply $x, [3], '$x.push (uninitialised scalar) autovivifies';
}

# `Nil.ords` warns and yields an empty Seq; `Nil.chrs` warns and yields a
# single null byte. The CX::Warn must be resumable, and the suspended call
# must continue with the resume value.
{
    CONTROL { when CX::Warn { pass 'Nil.ords warns'; .resume; } }
    is-eqv Nil.ords, ().Seq, 'Nil.ords gives an empty Seq';
}
{
    CONTROL { when CX::Warn { pass 'Nil.chrs warns'; .resume; } }
    is-deeply Nil.chrs, "\0", 'Nil.chrs gives a null byte';
}

done-testing;
