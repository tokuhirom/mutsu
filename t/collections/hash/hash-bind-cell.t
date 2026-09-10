use v6;
use Test;

plan 19;

# Phase 2 Stage 2: hash-key `:=` binds install shared ContainerRef cells
# (no BOUND_HASH_REF_SENTINEL back-references). Both sides of the bind
# alias one container, so writes through either side are mutually visible.

# Plain scalar source: write-through both ways
{
    my %h; my $x = 5;
    %h<k> := $x;
    $x = 9;
    is %h<k>, 9, 'scalar source: source write visible through hash key';
    %h<k> = 7;
    is $x, 7, 'scalar source: hash-key write visible through source';
}

# Element source (array): write-through both ways
{
    my @a = 1, 2, 3; my %h;
    %h<k> := @a[0];
    @a[0] = 99;
    is %h<k>, 99, 'array-element source: element write visible through hash key';
    %h<k> = 42;
    is @a[0], 42, 'array-element source: hash-key write visible through element';
}

# Element source (hash)
{
    my %src = a => 10; my %h;
    %h<k> := %src<a>;
    %src<a> = 20;
    is %h<k>, 20, 'hash-element source: element write visible through hash key';
    %h<k> = 30;
    is %src<a>, 30, 'hash-element source: hash-key write visible through element';
}

# Iteration sees the bound value (decont at the chokepoint)
{
    my %h; my $y = 1;
    %h<k> := $y;
    $y = 5;
    is %h.kv.join(","), "k,5", 'kv lists the current bound value';
    is %h.values.join(","), "5", 'values lists the current bound value';
}

# Cell reuse: a scalar already cell-bound elsewhere keeps all aliases shared
{
    my $x = 5;
    my @arr;
    @arr[0] := $x;
    my %h;
    %h<k> := $x;
    $x = 7;
    is @arr[0], 7, 'cell reuse: array alias follows';
    is %h<k>, 7, 'cell reuse: hash alias follows';
}

# Array source: mutation through the source is visible (incl. push)
{
    my %h; my @a = 1, 2;
    %h<k> := @a;
    @a.push(3);
    is-deeply %h<k>, [1, 2, 3], 'array source: push through source visible';
    %h<k>[0] = 9;
    is @a[0], 9, 'array source: deep write through hash key visible';
}

# Hash slice bind
{
    my %h; my $x = 1; my $y = 2;
    %h<a b> := ($x, $y);
    $x = 10;
    is %h<a>, 10, 'slice bind: source write visible';
    %h<a> = 5;
    is $x, 5, 'slice bind: hash-key write visible through source';
}

# BIND-KEY
{
    my %h; my $z = 3;
    %h.BIND-KEY('k', $z);
    $z = 8;
    is %h<k>, 8, 'BIND-KEY: source write visible';
}

# Assignment snapshots: copying a hash with bound entries deconts the cells
{
    my %hash = (:a<x>, :b<y>);
    my $var = "d";
    %hash<b> := $var;
    $var = "e";
    my %new = %hash;
    $var = "f";
    is %hash<b>, "f", 'original hash still aliases the source';
    is %new<b>, "e", 'assigned copy snapshot does not alias the source';
}

# :delete / :exists on a bound key leave the source untouched
{
    my %h; my $y = 1;
    %h<k> := $y;
    %h<k>:delete;
    is %h<k>:exists, False, 'bound key deleted';
    is $y, 1, 'source unchanged after :delete';
}

done-testing;
