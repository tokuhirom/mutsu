unit module BraceSubscriptRoutes;

sub routes(--> Hash:D) is export {
    { '/' => { 'GET' => 'imported-handler' } }
}
