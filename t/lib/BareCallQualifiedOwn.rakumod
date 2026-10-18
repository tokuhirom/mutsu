unit module BareCallQualifiedOwn;

need BareCallQualifiedOther;

sub bcq-name() { 'own' }

sub via-other() { BareCallQualifiedOther::bcq-name() }

our sub sequence() {
    my @r = bcq-name();
    @r.push: BareCallQualifiedOther::bcq-name();
    @r.push: bcq-name();
    @r.join(',')
}

our sub later() { via-other() ~ ',' ~ bcq-name() }
