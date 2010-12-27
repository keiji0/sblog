package SBlog::Page;

use utf8;
use strict;
use warnings;
use Encode;
use Storable qw(freeze thaw);
use SBlog::DB qw(db select);
use base qw(Exporter);
our @EXPORT_OK = qw(
  dbtopage printpage findpage
  savepage deletepage compilepage
  recache
);

sub dbtopage {
  my $a = shift;
  for my $k (keys %$a) {
    $k =~ /param|cache/ or $a->{$k} = decode_utf8 $a->{$k};
  }
  if ($a) {
    $a->{param} and $a->{param} = thaw $a->{param}
  }
  $a
}

sub printpage {
  my $page = shift;
  my $cache = thaw utf8::is_utf8($page->{cache}) ?
    encode_utf8($page->{cache}) : $page->{cache};
  while (my $a = shift @$cache) {
    if ($a eq 1) {
      package main;
      use SBlog::HTML qw(htmlescape cr2br);
      use SBlog::Config qw(config);
      use SBlog::DB qw(db);
      eval shift(@$cache) or die $@;
    } else {
      print $a;
    }
  }
}

sub findpage {
  my ($path) = @_;
  my $st = db->prepare('SELECT * FROM Page WHERE path=?') or die db->errstr;
  $st->execute($path) or die db->errstr;
  if (my $p = $st->fetchrow_hashref) {
    dbtopage $p
  } else {
    undef
  }
}

sub compilepage {
  use Text::Markdown qw(markdown);
  my $text = markdown shift;
  my @a;
  while ($text =~ /\s*\<textmacro\>\s*/) {
    $text = $';
    push @a, $`;
    if ($text =~ m|\s*\</textmacro\>\s*|) {
      push @a, 1;
      push @a, $`;
      $text = $';
    }
  }
  $text and push @a, $text;
  @a = grep { $_ } @a;
  freeze \@a;
}

sub savepage {
  my $page = shift or die 'page empty';
  my $opage = findpage $page->{path};
  if ($page->{id} or $opage) {
    $page->{id} = $opage->{id};
    db->do("UPDATE Page SET modified=datetime('now', 'localtime'), path=?, title=?, body=?, cache=?, param=? WHERE id=?;", undef,
           $page->{path}, $page->{title} || 'no title',
           $page->{body}, compilepage($page->{body}),
           freeze($page->{param}),
           $page->{id})
      or die db->errstr;
  } else {
    db->do("INSERT INTO Page(created, modified, path, title, body, cache, param)".
           "VALUES(datetime('now', 'localtime'), datetime('now', 'localtime'), ?, ?, ?, ?, ?);", undef,
           $page->{path}, $page->{title}, $page->{body}, compilepage($page->{body}), freeze($page->{param}))
      or die db->errstr;
  }
  db->commit or die db->errstr;
}

sub deletepage {
  my ($path) = @_;
  findpage $path or die $path.'は存在しません';
  db->do("DELETE FROM Page WHERE path = ?", undef, $path) or die db->errstr;
  db->commit or die db->errstr;
}

1
