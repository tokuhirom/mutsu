use Test;

plan 5;

# .return method triggers return from enclosing sub
{
    sub a { .return with 42 }
    is a, 42, '.return with value returns from sub';
}

# .return on junction does NOT auto-thread
{
    sub b { (1|2|3).return }
    isa-ok b, Junction, '.return on Junction returns the Junction itself';
}

# &return resolves to a callable
{
    my &r = &return;
    ok &r.defined, '&return resolves to a defined value';
}

# Proxied return: rebinding &return
{
    my $tracker = '';
    my &r = &return;
    sub f {
        my &return := -> $v {
            $tracker ~= 'PROXY';
            &r($v * 2);
        };
        return(21);
    }
    is f(), 42, 'proxied return produces correct value';
    is $tracker, 'PROXY', 'proxied return executes rebound closure';
}
