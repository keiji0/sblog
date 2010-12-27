package SBlog::HTML;

use utf8;
use strict;
use warnings;
use base qw(Exporter);
our @EXPORT_OK = qw(cr2br htmlescape ddateformat htmlhead htmlend);

sub cr2br { my $s = shift; $s =~ s|\n|<br />|g; $s }
sub htmlescape {
  my $s = shift or return '';
  $s =~ s/&/&amp;/g;
  $s =~ s/</&lt;/g;
  $s =~ s/>/&gt;/g;
  $s =~ s/\"/&quot;/g;
  $s
}

sub ddateformat {
  my ($sec, $min, $hour, $day, $mon, $year) = localtime shift;
  ($year+1900).'年'.($mon+1).'月'.$day.'日'
}

sub htmlhead {
  print <<"END";
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="ja" lang="ja">
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
  <meta http-equiv="Content-Script-Type" content="text/javascript" />
  <meta http-equiv="Content-Style-Type" content="text/css" />
END
  $_ and print "  ", $_, "\n" for @_;
  print <<END;
</head>
<body>
END
}

sub htmlend {
  print <<END
</body>
</html>
END
}

1
