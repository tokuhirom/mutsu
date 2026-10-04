use Test;
use nqp;

# `nqp::bindattr` / `nqp::getattr` on a value with roles mixed in address the
# role attributes in its role cell, which the role's methods read. Upstream
# NativeCall binds `$!entry-point` on a routine that does its `Native` role.

plan 2;

role R {
    has $!entry-point;
    method ep { $!entry-point }
}

my $r := sub { };
$r does R;
nqp::bindattr($r, $r.WHAT, '$!entry-point', 42);
is $r.ep, 42, 'a role method reads the bound attribute';
is nqp::getattr($r, $r.WHAT, '$!entry-point'), 42, 'nqp::getattr reads it back';
