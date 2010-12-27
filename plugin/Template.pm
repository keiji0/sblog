package SBlog::Template;

use utf8;
use strict;
use warnings;
use base qw(Exporter);
our @EXPORT_OK = qw(templateindex templatepage);

use SBlog::Config qw(config);
use SBlog::HTML qw(htmlescape htmlhead htmlend cr2br);

sub printnavi {
  sub sep() { ' <span class="sep">/</span> ' }
  my $path = shift;
  print '<div id="navi">';
  print '<a href="/">Top</a>'.sep;
  my $base = '';
  my @a = grep { $_ } split m|/|, $path;
  pop @a;
  for (grep { $_ } @a) {
    $base .= '/'.$_;
    print '<a href="'.$base.'">'.$_.'</a>'.sep;
  }
  print '</div>';
}

sub headlinks {
  sub headlinkgen {
    my ($p, $k) = @_;
    defined $p->{$k} and return '<link rel="'.htmlescape($k).'" '.$p->{$k}.' />';
    undef
  }
  my $p = shift;
  ((headlinkgen $p, 'up'),
   (headlinkgen $p, 'next'),
   (headlinkgen $p, 'prev'),
   (headlinkgen $p, 'index'),
   (headlinkgen $p, 'section'),
   (headlinkgen $p, 'topics'))
}

sub printgoogleanalytics { print config 'googleanalytics' }
sub rsslink() { '<link rel="alternate" type="application/rss+xml" href="/?mode=rss" title="RSS 1.0" />' }
sub headjscss {
  ((map { '<script src="'.$_.'"></script>' } @{ config('javascriptsrc') || [] }),
   (map { '<link rel="stylesheet" href="'.$_.'" />' } @{ config('csssrc') || [] }))
}

sub templateindex {
  my ($path, $pages) = @_;
  my $title = $path." index";
  htmlhead headjscss, rsslink, "<title>".$title."</title>";
  print '<div id="contents">';
  printnavi $path;
  print '<h1>'.$title.'</h1>';
  print '<ul>';
  print '<li>', '<a href="', $_->{path}, '">', $_->{title},'</a>', '</li>' for @$pages;
  print '</ul>';
  print '</div>';
  printgoogleanalytics;
  htmlend;
}

sub templatepage {
  sub commentform {
    sub commenttemplate {
      sub commentformat { join '<br />', map { htmlescape $_ } split /\n/, shift }
      my $c = shift;
      join '',
        '<p class="post" id="comment',$c->{id},'">',
          '<span class="body">', commentformat($c->{body}), '</span>',
          '<span class="date"> ', $c->{created}, '</span>',
          ' <label>By</label> <span class="name">', htmlescape($c->{name}), '</span>',
        '</p>'
    }
    use SBlog::Comment qw(getcomment);
    use SBlog::CGI qw(cgipath);
    print '<form action="'.cgipath.'#comment" method="POST" id="comment"><h2>コメント</h2>';
    print commenttemplate $_ for @{ getcomment(shift) };
    print <<END
    <p>
      <label for="commentname">名前: </label><br />
      <input type="text" name="name" id="commentname" />
      <input type="hidden" name="mode" value="comment" />
    </p>
    <p>
      <label for="commentbody">内容: </label><br />
      <textarea type="text" name=body id="commentbody" style="width:100%; height:10em;"></textarea>
    </p>
    <p>
      <input type="submit" name="投稿" />
    </p>
  </form>
END
  }
  use SBlog::Page qw(printpage);
  my $page = shift;
  my $param = $page->{param};
  htmlhead
    "<title>$page->{title}</title>",
    '<meta name="author" content="keiji0@gmail.com" />',
    ($param->{description} and '<meta name="description" content="'.htmlescape($param->{description}).'" />') || '',
    ($param->{keywords} and '<meta name="keywords" content="'.htmlescape($param->{keywords}).'" />') || '',
    headlinks($param),
    headjscss,
    rsslink;
  print '<div id="contents" class="page">';
  printnavi $page->{path};
  print "<h1>$page->{title}</h1>";
  printpage $page;
  commentform $page unless $param->{nocomment};
  print '<div class="meta">'.'作成日: '.$page->{created}.' 更新日: '.$page->{modified}.'</div>';
  print '</div>';
  printgoogleanalytics;
  htmlend;
}

1
