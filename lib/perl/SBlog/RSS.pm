package SBlog::RSS;

use utf8;
use strict;
use warnings;
use SBlog::Page qw(printpage);
use SBlog::HTML qw(htmlescape);

sub fdata {
  my $s = shift;
  $s =~ s/ /T/;
  $s =~ s/:\d\d$//;
  $s.'+09:00'
}

sub show {
  my ($data, $pages) = @_;
  print
    '<?xml version="1.0" encoding="utf-8" ?>',
    '<rdf:RDF ',
      'xmlns="http://purl.org/rss/1.0/" ',
      'xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" ',
      'xmlns:dc="http://purl.org/dc/elements/1.1/" ',
      'xml:lang="ja">';
  print
  '<channel rdf:about="', $data->{url}, '/?mode=rss">',
    '<title>', htmlescape($data->{title}), '</title>',
    '<link>', htmlescape($data->{url}), '</link>',
    '<description>', htmlescape($data->{description}), '</description>',
    '<items>',
      '<rdf:Seq>';
        print '<rdf:li rdf:resource="', $data->{url}, $_->{path}, '"/>' for @$pages; print
      '</rdf:Seq>',
    '</items>',
  '</channel>';
  for my $page (@$pages) {
    print
      '<item rdf:about="', $data->{url}, $page->{path}, '">',
        '<title>', htmlescape($page->{title}), '</title>',
        '<dc:date>', fdata($page->{created}) ,'</dc:date>',
        '<link>', $data->{url}, $page->{path}, '</link>';
         print '<description><![CDATA[';
         eval { printpage $page };
         print ']]></description>',
      '</item>';
  }
  print '</rdf:RDF>';
}

1
