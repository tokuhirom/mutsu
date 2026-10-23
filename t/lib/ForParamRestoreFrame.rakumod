class ForParamRestoreFrame {
    has @.panes;
    method focus($pane) { for @.panes -> $p { $p.so }; $pane }
}
class ForParamRestoreUI is export {
    method alert {
        my $f = ForParamRestoreFrame.new(panes => [1, 2]);
        my ($t, $p, $msg);
        ($msg, $p) = (5, 6);
        $f.focus($p);
        $p
    }
    method alert-init {
        my $f = ForParamRestoreFrame.new(panes => [1, 2]);
        my ($t, $p, $msg) = (4, 6, 5);
        $f.focus(6);
        $p
    }
}
