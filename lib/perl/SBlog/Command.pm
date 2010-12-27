package SBlog::Command;

use utf8;
binmode *STDOUT, ":utf8";
binmode *STDIN, ":utf8";
binmode *STDERR, ":utf8";
use strict;
use warnings;
use SBlog::DB qw(select);
use SBlog::Page qw(findpage savepage deletepage recache);
use base qw(Exporter);
our @EXPORT_OK = qw(commandexec);

sub parsepage {
  sub parseheadline { (shift) =~ /^([^: ]+): *(.+)/ }
  my $src = shift or die 'parsepage データを入力して下さい';
  my %p;
  my %page = ( param => \%p );
  while (<$src>) {
    m|^--| and goto body;
    if (my ($key, $val) = parseheadline $_) {
      $key =~ /^title|path|id$/ ?
        $page{$key} = $val :
        $p{$key} = $val;
    } else {
      die '不明なヘッダの形式です';
    }
  }
  die 'セパレートが記入されていません';
 body:
  my $s;
  $s .= $_ while <$src>;
  $page{body} = $s;
  \%page;
}

sub pagecontents {
  my $page = shift;
  my $s = '';
  $s .= 'id: '. ($page->{id}). "\n" if defined $page->{id};
  $s .= 'path: '. ($page->{path}). "\n";
  $s .= 'title: '. ($page->{title} || 'no title'). "\n";
  for (keys %{$page->{param}}) {
    $s .= $_. ': '. $page->{param}->{$_}. "\n";
  }
  $s .= "--\n";
  $s .= $page->{body} || '';
  $s;
}

sub genpath {
  my $category = shift;
  my ($sec, $min, $hour, $mday, $mon, $year, $wday, $yday, $isdst) = localtime time;
  sprintf '/%04d/%02d/%02d/%s', ($year+1900), ($mon+1), $mday, $category;
}

my $commandtable;
$commandtable = {
  touch => {
    usage => '[path]',
    desc => 'ページのテンプレートを出力する',
    command => sub {
      my ($args) = @_; 
      print pagecontents { path => $args->[0] || genpath('life') };
    },
  },

  genpath => {
    usage => '[path]',
    desc => 'ページの一意なパスを生成する',
    command => sub {
      my ($args) = @_;
      print genpath $args->[0] || '';
    },
  },

  cat => {
    usage => 'path',
    desc => 'ページの内容を出力する',
    command => sub {
      my ($args, $opt) = @_;
      my $page = findpage $args->[0] or die 'Page not found '.$args->[0];
      print pagecontents $page;
    },
  },

  exist => {
    usage => 'path',
    desc => '指定のページが存在するのか確認する',
    command => sub {
      my ($args, $opt) = @_;
      $args->[0] or die 'Exist command not found path.';
      exit(($args->[0] and findpage $args->[0]) ? 0 : 1);
    },
  },

  post => {
    usage => '< pagebody',
    desc => 'SBlogのページ形式で書かれたテキストを入力し保存する',
    command => sub { savepage parsepage *STDIN },
  },

  sync => {
    usage => '',
    desc => 'ブログをサーバーと同期する',
    command => sub { system '(cd $SBLOGDIR && make rsync)' },
  },

  ls => {
    usage => '',
    desc => '全てのページのパスを出力する',
    command => sub { print $_->{path}, "\n" for @{ select('path FROM Page Order By path') } }
  },

  recache => {
    usage => '',
    desc => 'キャッシュされた情報を更新します',
    command => sub { recache }
  },

  rm => {
    usage => 'path',
    desc => 'パスがしめすページを削除する',
    command => sub {
      my ($args, $options) = @_;
      findpage $_ or die $_.'は存在しません' for @$args;
      deletepage $_ for @$args;
    },
  },

  help => {
    usage => '',
    desc => 'コマンドのヘルプを出力する',
    command => sub {
      print sprintf(
        '%s %s'.($commandtable->{$_}->{desc} ? ': %s' : '%s')."\n",
        $_,
        $commandtable->{$_}->{usage},
        $commandtable->{$_}->{desc} || '',
      ) for sort { $a cmp $b } keys %$commandtable
    }
  },
};

sub commandexec {
  my $command = shift @ARGV;
  $command or die 'コマンドを入力して下さい';
  my %options;
  my @args;
  while (my $arg = shift @ARGV) {
    if ($arg =~ /^-(.+)/) {
      $options{$1} = shift @ARGV;
    } else {
      @args = @ARGV;
      unshift @args, $arg;
      last;
    }
  }
  $commandtable->{$command}->{command} or die '「'.($command).'」のコマンドは存在しません';
  $commandtable->{$command}->{command}->(\@args, \%options);
}

1
