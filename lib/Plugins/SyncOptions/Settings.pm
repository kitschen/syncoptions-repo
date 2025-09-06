package Plugins::SyncOptions::Settings;

# SyncOptions Copyright (c) 2008-2015 Peter Watkins
#
# SqueezeCenter Copyright (c) 2001-2007 Logitech.
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License,
# version 2.

use strict;
use base qw(Slim::Web::Settings);

use Slim::Utils::Prefs;
use Slim::Utils::DateTime;

my $prefs = preferences('plugin.SyncOptions');
my @oldPrefNames = ('PLUGIN_SyncOptions_unsync', 'PLUGIN_SyncOptions_wait', 'PLUGIN_SyncOptions_delay', 'PLUGIN_SyncOptions_precision');
my @prefNames = ('unsync', 'wait', 'delay', 'precision', 'restoreAfterUnsync', 'volumeSync', 'volumeSyncDelay', 'powerSync');

sub needsClient {
	return 0;
}

$prefs->migrate(1, sub {
	my ($class) = @_;
	# convert old pref
	for (my $i = 0; $i < scalar(@oldPrefNames); ++$i ) {
		my $newName = $prefNames[$i];
		my $oldName = $oldPrefNames[$i];
		$prefs->set($newName, Slim::Utils::Prefs::OldPrefs->get($oldName));
	}
	1;
});

sub name {
	return Slim::Web::HTTP::CSRF->protectName('PLUGIN_SYNCOPTIONS_BASIC_SETTINGS');
}

sub page {
	return Slim::Web::HTTP::CSRF->protectURI('plugins/SyncOptions/settings/basic.html');
}

sub prefs {
	return ($prefs, @prefNames );
}

sub handler {
	my ($class, $client, $params) = @_;

        # for bug 6873/change 19155
	if ($::VERSION ge '7.1') {
		$params->{'pw'}->{'pref_prefix'} = 'pref_';
	} else {
		$params->{'pw'}->{'pref_prefix'} = '';
	}

	### BUG - validate prefs!
	# might need new Jive menu
	Slim::Control::Jive::refreshPluginMenus();
	return $class->SUPER::handler($client, $params);
}

sub getPrefs {
	return $prefs;
}

1;

__END__
