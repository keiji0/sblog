package SBlog::Comment;

use strict;
use warnings;
use SBlog::DB qw(db select);
use base qw(Exporter);
our @EXPORT_OK = qw(postcomment getcomment);

sub postcomment {
  my ($page, $name, $body) = @_;
  db->do("INSERT INTO Comment(created, pageid, name, body) VALUES(datetime('now', 'localtime'), ?, ?, ?);", undef,
       $page->{id}, $name, $body)
    or die db->errstr;
  db->commit or die db->errstr;
}

sub getcomment {
  my ($page) = @_;
  select '* FROM Comment WHERE pageid=?;', $page->{id};
}

1
