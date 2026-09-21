unit module EcosystemSwayConfigPod;

=NAME sway-config-fixture

our $pod-name = $=pod[0].contents[0].contents[0];

sub pod-name() is export { $pod-name }
