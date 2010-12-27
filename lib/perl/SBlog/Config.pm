package SBlog::Config;

use strict;
use warnings;
use base qw(Exporter);
our @EXPORT_OK = qw(config configref);

my $data = -e 'var/config.pl' ? do('var/config.pl') : do('lib/config.pl');
sub config { my $k = shift; $data->{$k} }
sub configref() { $data }

1
