unit module Issue7692::Mid;
use Issue7692::Vars;

sub i7692-mid() is export {
    $issue7692-value ~ '/' ~ I7692Class.new.hi
}

sub i7692-nested() is export {
    sub inner() { $issue7692-value }
    inner()
}

sub i7692-block() is export {
    my $inner = { $issue7692-value ~ '/' ~ I7692Class.new.hi }
    $inner()
}
