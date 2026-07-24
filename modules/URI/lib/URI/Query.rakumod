unit class URI::Query
    does Positional
    does Associative
    does Iterable;

use IETF::RFC_Grammar::URI;
use URI::Escape;

our subset ValidQuery of Str
    where /^ <IETF::RFC_Grammar::URI::query> $/;

our enum HashFormat <None Mixed Singles Lists>;

has HashFormat $.hash-format is rw;
has ValidQuery $!query;
has Pair @!query-form;

our sub split-query(
    Str() $query,
    :$hash-format is copy = None,
) {
    $hash-format = None  if $hash-format eqv False;
    $hash-format = Lists if $hash-format eqv True;

    my @query-form = gather for $query.split(/<[&;]>/) {
        # when the query component contains abc=123 form
        when /'='/ {
            my ($k, $v) = .split('=', 2).map(&uri-unescape);
            take $k => $v;
        }

        # or if the component contains just abc
        when /./ {
            take $_ => True;
        }
    }

    given $hash-format {
        when Mixed {
            my %query-form;

            for @query-form -> $qp {
                if %query-form{ $qp.key }:exists {
                    if %query-form{ $qp.key } ~~ Array {
                        %query-form{ $qp.key }.push: $qp.value;
                    }
                    else {
                        %query-form{ $qp.key } = [
                            %query-form{ $qp.key },
                            $qp.value,
                        ];
                    }
                }
                else {
                    %query-form{ $qp.key } = $qp.value;
                }
            }

            return %query-form;
        }

        when Singles {
            return %(@query-form);
        }

        when Lists {
            my %query-form;
            %query-form{ .key }.push: .value for @query-form;
            return %query-form;
        }

        default {
            return @query-form;
        }
    }
}

# artifact form
our sub split_query(|c) { split-query(|c) }

multi method new(Str() $query, :$hash-format = Lists) {
    self.new(:$query, :$hash-format);
}

submethod BUILD(:$!query = '', :@!query-form, :$!hash-format = Lists) {
    die "When calling URI::Query.new set either :query or :query-form, but not both."
        if @!query-form and $!query;

    @!query-form = split-query($!query) if $!query;
}

method !value-for($key) {
    # TODO Is there a better way to make this list immutable than to use a
    # Proxy container?
    my $l = @!query-form.grep({ .key eq $key }).map({
        my $v = .value;
        Proxy.new(
            FETCH => method () { $v },
            STORE => method ($n) { X::Assignment::RO.new(:value($v)).throw },
        );
    }).List;

    given $!hash-format {
        when Mixed   { $l.elems == 1 ?? $l[0] !! $l }
        when Singles { $l.first(:end) }
        default      { $l }
    }
}

method of() { Pair }
method iterator(URI::Query:D:) { @!query-form.iterator }

# positional context
method AT-POS(URI::Query:D: $i)     { @!query-form[$i] }
method EXISTS-POS(URI::Query:D: $i) { @!query-form[$i]:exists }
method ASSIGN-POS(URI::Query:D: $i, Pair $p) {
    $!query = Nil;
    @!query-form[$i] = $p;
}
method DELETE-POS(URI::Query:D: $i) {
    $!query = Nil;
    @!query-form[$i]:delete
}

# associative context
method AT-KEY(URI::Query:D: $k)     { self!value-for($k) }
method EXISTS-KEY(URI::Query:D: $k) { @!query-form.first({ .key eq $k }).defined }
method ASSIGN-KEY(URI::Query:D: $k, $value) {
    $!query = Nil;
    @!query-form .= grep({ .key ne $k });

    given $!hash-format {
        when Singles {
            @!query-form.push: $k => $value;
        }
        default {
            @!query-form.append: $value.list.map({ $k => $^v });
        }
    }

    $value
}
method DELETE-KEY(URI::Query:D: $k) {
    $!query = Nil;
    my $r = self!value-for($k);
    my @ps = @!query-form .= grep({ .key ne $k });
    $r;
}

# Hash-like readers
method keys(URI::Query:D:) { @!query-form.map(*.key) }
method values(URI::Query:D:) { @!query-form.map(*.value) }
method kv(URI::Query:D:) { @!query-form.map(*.kv).flat }
method pairs(URI::Query:D:) { @!query-form }

# Array-like manipulators
method pop(URI::Query:D:) { $!query = Nil; @!query-form.pop }
method push(URI::Query:D: |c) { $!query = Nil; @!query-form.push: |c }
method append(URI::Query:D: |c) { $!query = Nil; @!query-form.append: |c }
method shift(URI::Query:D:) { $!query = Nil; @!query-form.shift }
method unshift(URI::Query:D: |c) { $!query = Nil; @!query-form.unshift: |c }
method prepend(URI::Query:D: |c) { $!query = Nil; @!query-form.prepend: |c }
method splice(URI::Query:D: |c) { $!query = Nil; @!query-form.splice: |c }

# Array-like readers
method elems(URI::Query:D:) { @!query-form.elems }
method end(URI::Query:D:) { @!query-form.end }

method Bool(URI::Query:D: |c) { @!query-form.Bool }
method Int(URI::Query:D: |c) { @!query-form.Int }
method Numeric(URI::Query:D: |c) { @!query-form.Numeric }

multi method query(URI::Query:D:) {
    return $!query with $!query;
    $!query = @!query-form.grep({ .defined }).map({
        if .value eqv True {
            "{uri-escape(.key)}="
        }
        else {
            "{uri-escape(.key)}={uri-escape(.value)}"
        }
    }).join('&');

    # If there's only one pair => True, drop the =
    # so end with "foo" not "foo=" but keep "foo=&bar="
    # If someone is pickier, they should set $!query directly.
    $!query ~~ s/'=' $// unless $!query ~~ /'&'/;

    $!query;
}

multi method query(URI::Query:D: Str() $new) {
    $!query = $new;
    @!query-form = split-query($!query);
    $!query;
}

multi method query-form(URI::Query:D:) { @!query-form }

multi method query-form(URI::Query:D: *@new, *%new) {
    $!query = Nil;
    @!query-form = flat %new, @new;
}

# string context
multi method gist(URI::Query:D: --> Str ) { $.query }
multi method Str(URI::Query:D: --> Str ) { $.gist }

# vim: expandtab shiftwidth=4
