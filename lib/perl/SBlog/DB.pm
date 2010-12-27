package SBlog::DB;

use strict;
use warnings;
use DBI;
use Encode;
use base qw(Exporter);
our @EXPORT_OK = qw(db select hashdecode);

sub dbpath() { 'var/main.db' }

my $db = DBI->connect("dbi:SQLite:dbname=".dbpath, undef, undef, {
  AutoCommit => 0,
  RaiseError => 1,
  # sqlite_unicode => 1
}) or die $DBI::errstr;

sub hashdecode {
  my $p = shift;
  $p->{$_} = decode_utf8 $p->{$_} for keys %$p;
}

sub select {
  my ($sql, @args) = @_;
  my $a = $db->selectall_arrayref('SELECT '.$sql, { Slice => {} }, @args) or die $db->errstr;
  hashdecode $_ for @$a;
  $a;
}

sub db() { $db }
END { db->disconnect or die $! }

1
