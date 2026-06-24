unit class Tubu::Request;

# Wraps a P6W-style %env hash into a convenient request object.
has %.env;
has %.captures;   # path captures from the matched route (:id -> value)

method new(%env) {
    self.bless(env => %env);
}

method method() { %.env<REQUEST_METHOD> // 'GET' }
method path()   { %.env<PATH_INFO> // '/' }
method query-string() { %.env<QUERY_STRING> // '' }

method header(Str $name) {
    my $key = 'HTTP_' ~ $name.uc.subst('-', '_', :g);
    return %.env{$key};
}

sub url-decode(Str $s) {
    my $t = $s.subst('+', ' ', :g);
    $t .= subst(/'%' (<[0..9A..Fa..f]> ** 2)/, -> $m { chr(:16(~$m[0])) }, :g);
    return $t;
}

sub parse-pairs(Str $s) {
    my %h;
    return %h unless $s;
    for $s.split('&') -> $pair {
        next unless $pair;
        my ($k, $v) = $pair.split('=', 2);
        %h{url-decode($k)} = url-decode($v // '');
    }
    return %h;
}

method query() {
    return parse-pairs(self.query-string);
}

method body() {
    return %.env<p6w.input.body> // '';
}

method body-params() {
    return parse-pairs(self.body);
}

# merged query + body params
method params() {
    my %p = self.query;
    for self.body-params.kv -> $k, $v {
        %p{$k} = $v;
    }
    return %p;
}

method param(Str $name) {
    return self.params{$name};
}

method cookies() {
    my %c;
    my $raw = self.header('Cookie') // '';
    return %c unless $raw;
    for $raw.split(/\s* ';' \s*/) -> $pair {
        next unless $pair;
        my ($k, $v) = $pair.split('=', 2);
        %c{$k.trim} = $v // '';
    }
    return %c;
}
