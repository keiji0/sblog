package SBlog;

use utf8;
use strict;
use warnings;
use base qw(Exporter);
our @EXPORT_OK = qw(start);

sub start {
  package main;
  use SBlog::DB qw(db select);
  use SBlog::Config qw(config configref);
  use SBlog::CGI qw(showtext httphead cgipath notfound requestmethod cgiparam);
  use SBlog::Page qw(findpage);

  sub _loadtemplate {
    require 'plugin/Template.pm' or die $@;
  }
  sub _showrss {
    my $pages = select '*, created FROM Page WHERE path LIKE (?) ORDER BY created DESC LIMIT 15', cgipath.'%';
    if ($#$pages > -1) {
      require 'SBlog/RSS.pm';
      httphead 'Content-Type: application/xml';
      SBlog::RSS::show(configref, $pages);
    } else {
      notfound;
    }
  }
  sub _showpage {
    _loadtemplate;
    my $page = shift;
    httphead;
    SBlog::Template::templatepage($page);
  }
  sub _postcomment {
    use SBlog::Comment qw(postcomment);
    my $page = shift;
    my ($name, $body) = (cgiparam('name'), cgiparam('body'));
    if ($name and $body) {
      postcomment $page, $name, $body;
      _showpage $page;
    } else {
      showtext 'コメントに名前と内容を入力して下さい'
    }
  }
  sub _showindex {
    sub _getindexpage {
      select 'path, title, strftime(\'%Y年%m月%d日\', created) AS created'.
        ' FROM Page'.
        ' WHERE path LIKE (?) ORDER BY path',
        (shift).'%';
    }
    my $path = shift;
    my $pages = _getindexpage $path;
    if ($#$pages > -1) {
      _loadtemplate;
      httphead;
      SBlog::Template::templateindex($path, $pages);
    } else {
      notfound;
    }
  }
  my $mode = cgiparam('mode') || '';
  if ('rss' eq $mode) {
    _showrss;
  } elsif (my $page = findpage cgipath) {
    if (requestmethod eq 'POST') {
      if ('comment' eq $mode) {
        _postcomment $page;
      } else {
        showtext '「'.$mode.'」は無効なモードです'
      }
    } else {
      _showpage $page;
    }
  } else {
    _showindex cgipath;
  }
}

1
