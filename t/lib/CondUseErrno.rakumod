use if;
use CondUseErrnoUnix:if(True);
sub chk() is export { errno.WHAT.^name }
