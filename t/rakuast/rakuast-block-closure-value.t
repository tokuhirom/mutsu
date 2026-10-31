use experimental :rakuast;
use Test;

# static::Analyzer reads the closure of a block it meets at CHECK time even when
# the block names lexicals of the (not yet run) unit, and resolves the special
# variables and the statement routines (#15610). Expected values are rakudo
# 2026.09's.

plan 7;

my $x = 1;
my $s = supply { emit 1 };
$s.tap(-> $n { $x = "bad" });
my Int $given = do given 1 { when Int { $_ } };

my (@metas, @topics, @core);
CHECK {
    sub walk($n) {
        return unless $n ~~ RakuAST::Node;
        @metas.push: $n.meta-object if $n ~~ RakuAST::PointyBlock;
        @topics.push: $n if $n ~~ RakuAST::Var::Lexical && $n.name eq '$_';
        @core.push: $n if $n ~~ RakuAST::Call::Name && $n.name.canonicalize eq 'emit';
        $n.visit-children({ walk($_) });
    }
    walk($*CU);
}

is @metas.elems, 1, 'one pointy block';
ok @metas[0] ~~ Block, 'its meta-object is a Block';
is @metas[0].signature.raku, ':($n)', '... with its signature';
ok @topics.elems >= 1, '$_ is used';
ok @topics.all.is-resolved, '$_ resolves';
is @topics[0].resolution.^name, 'RakuAST::VarDeclaration::Implicit::Special', '... to an implicit special declaration';
ok @core[0].is-resolved && @core[0].resolution ~~ RakuAST::Declaration::External::Setting, 'emit is a routine of the setting';
