use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'Algorithm::DiffMatchPatch',
    license             => 'perl',
    dist_author         => q{Ben Montgomery <b3nm0nty@gmail.com>},
    dist_version_from   => 'lib/Algorithm/DiffMatchPatch.pm',
    build_requires => {
        'Test::More' => 0,
    },
    add_to_cleanup      => [ 'Algorithm-DiffMatchPatch-*' ],
    create_makefile_pl => 'traditional',
);

$builder->create_build_script();
