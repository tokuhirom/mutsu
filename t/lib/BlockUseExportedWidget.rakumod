unit module BlockUseExportedWidget;

class Widget is export { method who { "module-widget" } }

sub make-widget() is export { Widget.new }
