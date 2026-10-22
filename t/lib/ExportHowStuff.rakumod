sub foo is export { True }

sub baz is export { True };

sub derp is export(:rob-schnider){ True }
sub herp is export(:rob-schnider){ True }


sub EXPORT($opt?){

    {
        '&bar' => sub { True },
        ( '&opt' => sub { True } with $opt )
    }

};

class AGlobalishSymbol {}

my package EXPORTHOW {
    package DECLARE {
        constant pokemon = Metamodel::ClassHOW;
    }
    # package SUPERSEDE {
    #     OUR::<grammar> =  Metamodel::ClassHOW
    # }
}

# vim: expandtab shiftwidth=4
