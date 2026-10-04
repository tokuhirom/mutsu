unit module BlockPhasersSplit;

our @log;

LEAVE { @log.push('leave') }

@log.push('body');

sub phaser-log() is export { @log.join(',') }
