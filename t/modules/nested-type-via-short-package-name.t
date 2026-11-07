use v6;
use lib 't/lib-nested-type-short-qualified';
use NestedQual;
use Test;

# `our class Packet::Sub` in a `unit package`'s file is reachable from the
# importer as `Packet::Sub`, and so is the class's nested type
# (`Packet::Sub::Inner`). Found with Protocol::MQTT's t/04-client.
# Identical under raku.

plan 3;

is Packet::Sub.^name, 'NestedQual::Packet::Sub', 'the short package name resolves';
is Packet::Sub::Inner.^name, 'NestedQual::Packet::Sub::Inner', 'so does its nested type';
is Packet::Sub::Inner.new(:t(3)).t, 3, 'which can be instantiated';
