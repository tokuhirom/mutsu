use Test;

plan 32;

# An itemized hash (`$(%h)`, `%h.item`, `${...}`) is still THE hash: element
# assignment through a scalar that holds it, through the array element that
# holds it, and through a `for` loop variable bound to that element writes
# into the shared hash. `.item` used to wrap the hash in a `Scalar`, which the
# subscript-assign lanes did not recognise as a hash, so they replaced the
# variable with a fresh `{k => v}` and the write was lost.
# (JSON::RPC: `for $responses.list -> $r { $r<out> = ... }` after `from-json`.)

# --- a scalar holding `%h.item` / `$(%h)` -------------------------------------
{
    my %hh = id => 1;
    my $h = %hh.item;
    $h<a> = 1;
    is $h.raku, '${:a(1), :id(1)}', '%h.item scalar: write keeps the existing keys';
    is %hh.raku, '{:a(1), :id(1)}', '%h.item scalar: write reaches the source hash';
}
{
    my %hh = id => 1;
    my $h = $(%hh);
    $h<a> = 1;
    is $h.raku, '${:a(1), :id(1)}', '$(%h) scalar: write keeps the existing keys';
    is %hh.raku, '{:a(1), :id(1)}', '$(%h) scalar: write reaches the source hash';
}
{
    my $h = ${id => 1};
    $h<a> = 1;
    is $h.raku, '${:a(1), :id(1)}', '${...} literal: write keeps the existing keys';
}
{
    my $h = {id => 1}.item;
    $h<a> = 1;
    is $h.raku, '${:a(1), :id(1)}', '{...}.item: write keeps the existing keys';
}
{
    my %hh = id => 1;
    my $g = %hh;
    my $h = $g.item;
    $h<a> = 1;
    is %hh.raku, '{:a(1), :id(1)}', '$g.item: write reaches the source hash';
}
{
    my %hh = id => 1;
    my $h := $(%hh);
    $h<a> = 1;
    is $h.raku, '${:a(1), :id(1)}', 'bound to $(%h): write keeps the existing keys';
    is %hh.raku, '{:a(1), :id(1)}', 'bound to $(%h): write reaches the source hash';
}

# --- an array element holding an itemized hash --------------------------------
{
    my %hh = id => 1;
    my @c = $(%hh),;
    my $h = @c[0];
    $h<a> = 1;
    is @c.raku, '[{:a(1), :id(1)},]', 'my $h = @c[0]: write is visible in the array';
    is %hh.raku, '{:a(1), :id(1)}', 'my $h = @c[0]: write reaches the source hash';
}
{
    my %hh = id => 1;
    my @c = $(%hh),;
    @c[0]<a> = 1;
    is @c.raku, '[{:a(1), :id(1)},]', '@c[0]<a> = 1 keeps the element\'s other keys';
    is %hh.raku, '{:a(1), :id(1)}', '@c[0]<a> = 1 reaches the source hash';
}
{
    my %hh = id => 1;
    my @d = %hh.item,;
    @d[0]<b> = 2;
    is @d.raku, '[{:b(2), :id(1)},]', '@d[0]<b> = 2 on a %h.item element';
    my $x = @d[0];
    $x<c> = 3;
    is @d.raku, '[{:b(2), :c(3), :id(1)},]', 'scalar read from a %h.item element writes through';
    is %hh.raku, '{:b(2), :c(3), :id(1)}', 'and the source hash sees both writes';
}

# --- `for` over such an array -------------------------------------------------
{
    my %hh = id => 1;
    my @c = $(%hh),;
    for @c -> $h2 { $h2<out> = 'Y' }
    is @c.raku, '[{:id(1), :out("Y")},]', 'for @c -> $h: write through the loop variable';
    is %hh.raku, '{:id(1), :out("Y")}', 'for @c -> $h: write reaches the source hash';
}
{
    my %hh = id => 1;
    my @c = $(%hh),;
    for @c { $_<out> = 'Y' }
    is @c.raku, '[{:id(1), :out("Y")},]', 'for @c { $_<k> = v }: write through the topic';
}
{
    # Two itemized hashes in one array, each written through its own variable.
    my %p = n => 1;
    my %q = n => 2;
    my @c = $(%p), $(%q);
    for @c -> $r { $r<seen> = True }
    is %p.raku, '{:n(1), :seen(Bool::True)}', 'for over two itemized hashes: first';
    is %q.raku, '{:n(2), :seen(Bool::True)}', 'for over two itemized hashes: second';
}

# --- the other mutators on the same shape (already worked; pinned) ------------
{
    my %hh = id => 1;
    my @c = $(%hh),;
    @c[0].push('z' => 1);
    is %hh.raku, '{:id(1), :z(1)}', '.push through an itemized element';
    @c[0]<id>:delete;
    is %hh.raku, '{:z(1)}', ':delete through an itemized element';
}

# --- itemization is still observable, and still one item ----------------------
{
    my %h = a => 1;
    is %h.item.raku, '${:a(1)}', '%h.item renders itemized';
    is $(%h).raku, '${:a(1)}', '$(%h) renders itemized';
    is (1, %h.item, 3).elems, 3, '%h.item is ONE item in a list';
    is [%h.item].elems, 1, '%h.item is ONE element of an array literal';
    is (%h.item,).raku, '(${:a(1)},)', 'a List holding %h.item renders the container';
}

# --- a method call decontainerizes its invocant -------------------------------
{
    my %h = a => 1;
    is %h.item.Hash.raku, '{:a(1)}', '%h.item.Hash is the hash, not the holder';
    my $s = %h;
    is $s.Hash.raku, '{:a(1)}', 'a `$`-held hash .Hash is the hash, not the holder';
    ok %h.item.Hash.WHICH eq %h.WHICH, '.Hash is identity over the same hash';
}

# --- an itemized hash stored in a hash value ----------------------------------
{
    my %hh = id => 1;
    my %outer = k => $(%hh);
    my $v = %outer<k>;
    $v<a> = 1;
    is %hh.raku, '{:a(1), :id(1)}', 'scalar read from a hash value writes through';
}
