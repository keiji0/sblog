package SBlog::CGI;

use utf8;
binmode *STDOUT, ":utf8";
use strict;
use warnings;
use Encode;
use base qw(Exporter);
our @EXPORT_OK = qw(requestmethod httphead showtext notfound cgipath cgiparam);

sub requestmethod { $ENV{REQUEST_METHOD} || 'GET' }
sub httphead { print "Content-Type: @{[ shift || 'text/html' ]}\n\n" }
sub showtext { httphead 'text/plain; charset="utf-8"'; print shift }
sub notfound { showtext '404 not found' }

{
  my $s = pencode($ENV{PATH_INFO} || '');
  $s =~ m|^/| or $s = '/'.$s;
  sub cgipath() { $s }
}  

sub rawparam {
  requestmethod eq 'GET' and return $ENV{QUERY_STRING} || '';
  read STDIN, my $r, $ENV{CONTENT_LENGTH};
  $r;
}

sub pencode {
  my $s = shift;
  $s =~ tr/+/ /;
  $s =~ s/%(..)/pack("H2", $1)/eg;
  $s = decode_utf8 $s;
  $s
}

sub parseparam {
  my %p;
  for (split /&/, rawparam) {
    my ($k, $v) = split /=/;
    $p{pencode($k)} = pencode $v;
  }
  \%p;
}

{
  my $param = parseparam;
  sub cgiparam { $param->{$_[0]} }
}

1
