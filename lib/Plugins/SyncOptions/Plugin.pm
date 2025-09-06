# SyncOptions copyright (c) 2008-2019 by Peter Watkins (peterw@tux.org) 
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License,
# version 2, which should be included with this software.
# 
# Portions of this code might be derived from code with the 
# following copyright message and released under the same license terms:
#
# SliMP3 Server Copyright (C) 2001 Sean Adams, Slim Devices Inc.
# SlimServer Copyright (c) 2001-2006 Sean Adams, Slim Devices Inc.
# 

package Plugins::SyncOptions::Plugin;

use strict;

use Slim::Control::Request;
use Slim::Utils::DateTime;
use Slim::Utils::Strings qw (string);
use Slim::Utils::Misc qw( msg );
use Slim::Utils::Prefs;
use File::Spec::Functions qw(:ALL);
use POSIX qw(strftime);

# create a logging object
my $log = Slim::Utils::Log->addLogCategory({
	'category'     => 'plugin.SyncOptions',
	'defaultLevel' => 'ERROR',
	'description'  => getMyDisplayName(),
});

# prefs
my $prefs = preferences('plugin.SyncOptions');
my $serverPrefs = preferences('server');

# ------------------------------ Settings --------------------------------
# whether we should default to unsyncing players when they power off
my $defaultUnsyncAtPowerOff = 0;
# whether we should default to waiting until the next song to sync
my $defaultWaitToSync = 0;
# how many seconds into a file track a player can join without waiting
my $defaultSyncDelayMax = 5;
# how often to check for new track when at least one joiner waits and 'newsong' event fires
my $defaultPrecisionSecs = 0.5;
# whether volume change affect synced peers or only this player
my $defaultVolumeSync = 0;
# how long after joining a sync group before applying relative volume changes
my $defaultVolumeSyncDelay = 10;
# whether power on/off should sync across all players in a sync group
my $defaultPowerSync = 0;

# ------------------------------ Settings --------------------------------

# per-client variables for operation
my %lastSongChange;
my %lastSong;
my %timerSet;
my %waitingPlayers;
my %waitingForName;
my %waitingForId;
my %forceSync;
my %inOurMode;
my %whichString;
my %initialSyncVolume;
my %volumeSyncOverride;
my %iPeng;

# global variables
my $originalPowerCommand;	# need to keep track of original
my $originalSyncCommand;	# need to keep track of original
my $callbackset = 0;
my $modeString = 'PLUGIN.SyncOptions';
my $isNewStreaming = 0;
my $syncingVolume = 0;
my $originalMixerVolumeCommand;

use vars qw($VERSION);
$VERSION = &vlVersion();

# a constant string to identify this screensaver
my $screensaverSuffix = 'plugin_SyncOptions';
my $screensaverName = 'SCREENSAVER.'.$screensaverSuffix;

# whether we're enabled (the callback remains in the function 
# stack after "disabling" the plugin, so we need to keep track of this)
my $pluginEnabled = 0;

# setup routine
sub initPlugin {
	# note that we should act
	if ( $callbackset == 0 ) {
		if (! $::noweb) {
			require Plugins::SyncOptions::Settings;
		}
		$log->debug("wrapping button functions\n");
		# trap 'sync' so we can deny it
		$originalSyncCommand = Slim::Control::Request::addDispatch(['sync','_indexid-'],[1, 0, 0, \&SyncOptions_syncCommand]);
		if ( (!defined($originalSyncCommand)) || (ref($originalSyncCommand ) ne 'CODE') ) {
			$log->fatal("problem wrapping sync command!\n");
		}
		# trap 'power' so we can unsync
		# try original power command definition: only a _newvalue arg
		if ( substr($::VERSION,0,3) lt 7.4 ) {
			$originalMixerVolumeCommand = Slim::Control::Request::addDispatch(['mixer', 'volume', '_newvalue'],[1, 0, 0, \&SyncOptions_mixerVolumeCommand]);
			$originalPowerCommand = Slim::Control::Request::addDispatch(['power','_newvalue'],[1, 0, 0, \&SyncOptions_powerCommand]);
			if ( (!defined($originalPowerCommand)) || (ref($originalPowerCommand ) ne 'CODE') ) {
				# use new power command definition: also a _noplay arg
				$originalPowerCommand = Slim::Control::Request::addDispatch(['power','_newvalue','_noplay'],[1, 0, 0, \&SyncOptions_powerCommand]);
				if ( (!defined($originalPowerCommand)) || (ref($originalPowerCommand ) ne 'CODE') ) {
					$log->fatal("problem wrapping power command!\n");
				}
			}
		} else {
			# use new power command definition: also a _noplay arg
			$originalPowerCommand = Slim::Control::Request::addDispatch(['power','_newvalue','_noplay'],[1, 0, 1, \&SyncOptions_powerCommand]);
			$originalMixerVolumeCommand = Slim::Control::Request::addDispatch(['mixer', 'volume', '_newvalue'],[1, 0, 1, \&SyncOptions_mixerVolumeCommand]);
			if ( (!defined($originalPowerCommand)) || (ref($originalPowerCommand ) ne 'CODE') ) {
				$log->fatal("problem wrapping power command!\n");
			}
		}
		if (! $::noweb) {
			# global settings module
			Plugins::SyncOptions::Settings->new;
		}
		# initialize prefs
		&getConfig();
		# watch for volume changes
		$serverPrefs->setChange(\&volumeChange, 'volume');
		# watch for relative volume sync pref changes
		$prefs->setChange(\&volumePrefChange, 'volumeSync');
		# watch for power sync pref changes
		$prefs->setChange(\&powerPrefChange, 'powerSync');
		# watch for new players
		Slim::Control::Request::subscribe( \&newPlayerCheck, [['client']],[['new']]);
		# watch for new songs
		Slim::Control::Request::subscribe( \&newSong, [['playlist']],[['newsong']]);
		# subscribe for alarm sound to removeFromWaitingPlayers
		Slim::Control::Request::subscribe( \&alarmSounding, [['alarm']],[['sound']]);
		# register our UI mode
		Slim::Buttons::Common::addMode( $modeString, getFunctions(), \&setWaitingMode );
		# tell SM what prefs to ignore
		Slim::Utils::Timers::setTimer('moot', (Time::HiRes::time() + 1), \&settingsManager_register);
		# is this new-streaming?
		if ( substr($::VERSION,0,3) ge '7.3' ) {
			$isNewStreaming = 1;
		}
		# ContextMenu function
		Slim::Utils::Timers::setTimer('moot', (Time::HiRes::time() + 1), \&contextmenu_register);
		Slim::Buttons::Common::addMode( 'PLUGIN.SyncOptions.VolumeSyncOverride', &getFunctions, \&setMode );
		$callbackset = 1;
	}
	$pluginEnabled = 1;
	$log->debug("SyncOptions enabled\n");
}

sub SyncOptions_mixerVolumeCommand {
	$log->debug("SyncOptions_mixerVolumeCommand running\n");
	my @args = @_;
	my $request = $args[0];
	my $client = $request->client();
	my $id = $client->id();
	# if iPeng, return to prevent wated cycles or loops
	my $itest = $request->getParam('ipeng');
	if ( defined($itest) && ($itest == 1) ) {
		$log->debug("noting that the volume change request for $id came from iPeng");
		$iPeng{$id} = 1;
	}
	my $rc = &$originalMixerVolumeCommand(@args);
	$log->debug("clearing iPeng volume change request note for $id");
	$iPeng{$id} = undef;
	return $rc;
}

sub newPlayerCheck {
	my $request = shift;
	my $client = $request->client();
	if ( defined($client) ) {
		my @peers = &getSyncPeers($client);
		if ( scalar(@peers) > 0 ) {
			my $id = $client->id();
			if (! defined($initialSyncVolume{$id}) ) {
				&setInitialSyncVolume($client, 0);
			}
			foreach my $p ( @peers ) {
				my $pid = $p->id();
				if (! defined($initialSyncVolume{$pid}) ) {
					&setInitialSyncVolume($p, 0);
				} 
			}
		} else {
			&unsetInitialSyncVolume($client);
		}
	}
}

sub volumePrefChange {
	my ($pref, $val, $client) = @_;
	my @players = Slim::Player::Client::clients();
	if ( $val == 0 ) { 
		# forget the per-player info, as we're not syncing
		# BUG? should we remember this anyway for CM use?
		# the CM mechanism should probably recompute the ratios
		# when the CM change is requested
		foreach my $p ( @players ) {
			&unsetInitialSyncVolume($p);
		}
		return; 
	}
	# if we're here, we now DO care about relative volumes
	my %masterids;
	my @masters;
	# 	find sync groups
	foreach my $p ( @players ) {
		my $syncController = $p->controller;
		my $m = $syncController->master();
		my $md = $m->id();
		if (!defined($masterids{$md})) {
			push @masters, $m;
			&setInitialSyncVolume($m, 0);
		}
	}
	# 	set relative volumes
	foreach my $m ( @masters ) {
		my $sc = $m->controller;
		foreach my $p ( $sc->allPlayers() ) {
			&setInitialSyncVolume($p, 0);
		}
	}
}

sub powerPrefChange {
	my ($pref, $val, $client) = @_;
	$log->debug("power sync preference changed to $val");
	# No immediate action needed - the preference will be checked when power commands are issued
}

sub volumeChange {
	my ($pref, $val, $client) = @_;
#print STDERR "volumeChange running\n";
	my $id = $client->id();
	if (! defined($initialSyncVolume{$id}) ) {
		# we shouldn't sync volumes yet
		$log->debug("player $id has no volume info, too soon to sync volume");
		return;
	} 
	my $coreVolSync = $serverPrefs->client($client)->get('syncVolume');
	if ( $coreVolSync ) {
		$log->debug("player $id has the core volume sync enabled, SyncOptions will not change its peers volume");
		return;
	}
	# if already syncing volume, return so we don't recurse
	if ( $syncingVolume ) { 
		$log->debug('already syncing volumes');
		return; 
	}
	if ( defined($iPeng{$id}) && ($iPeng{$id} == 1) ) {
		$log->debug('iPeng: not bothering to sync volume group');
		$iPeng{$id} = undef;
		return;
	}
	my $setting = $prefs->get('volumeSync');
	my $override = $volumeSyncOverride{$id};
	if ( defined($override) ) { $setting = $override; }
	# if not doing relative volume sync, return
	if ( $setting != 1 ) { 
		$log->debug('not supposed to sync volume group');
		return; 
	}
	# calculate ratio of new volume to old volume 
	my $oldVolume = $initialSyncVolume{$id};
	my $ratio = $val / $oldVolume;
	# note that we're syncing volumes
	$syncingVolume = 1;
	# foreach peer
	my $resync = 0;
	PEER: foreach my $p ( &getSyncPeers($client) ) {
		my $pid = $p->id();
		my $coreVolSync = $serverPrefs->client($p)->get('syncVolume');
		if ( $coreVolSync ) {
			$log->debug("player $pid has the core volume sync enabled, SyncOptions will not change its volume");
			next PEER;
		}
		$log->debug("$id from $oldVolume to $val -- consider changing $pid volume");
		# get peer's initial volume
		if (! defined($initialSyncVolume{$pid}) ) {
			&setInitialSyncVolume($p, 0);
		} 
		# multiple by ratio
		my $pold = $initialSyncVolume{$pid};

		# TODO - the below calculates based on the "volume" pref's visible value (0-100)
		# but it might be better to calculate based on attenuation (-NN dB to 0 dB)
		# since at least Boom (also Radio?) attenuates more at lowest visible values than Touch/Classic/etc.
		my $new = int(($pold * $ratio) + 0.5);
		my $cv = $serverPrefs->client($p)->get('volume');

		# change the volume if needed
		if ( int($cv) != $new ) {
			$log->info("$id from $oldVolume to $val means $pid goes from $pold to $new");
			$p->execute( [ 'mixer','volume',($new < 100 ? $new : 100) ] );
		}
		if ( $new > 100 ) {
			$resync = 1;
		}
	}
	# resync if any players would have gone past 100
	if ( $resync == 1 ) {
		&resync($client);
	}
	# note that we're done syncing volumes
	$syncingVolume = 0;
}

sub resync($) {
	my $client = shift;
	&setInitialSyncVolume($client, 0);
	foreach my $p ( &getSyncPeers($client) ) {
		&setInitialSyncVolume($p, 0);
	}
}

sub getSyncPeers($) {
	my $client = shift;
	my @rc = ();
	my @rcids = ();
	my $syncController = $client->controller;
	if (! defined($syncController) ) { return @rc; }
	my @allPlayers = $syncController->allPlayers();
	my $id = $client->id();
	foreach my $p ( @allPlayers ) {
		if ( $p->id() ne $id ) {
			push @rc, $p;
			push @rcids, $p->id();
		}
	}
	$log->debug("peers for $id are ".join(', ',@rcids));
	return @rc;
}

sub syncPowerAcrossGroup($$) {
	my ($client, $powerState) = @_;
	my $id = $client->id();
	my @peers = &getSyncPeers($client);
	
	$log->info("syncing power state $powerState for $id across ".scalar(@peers)." peers");
	
	foreach my $peer ( @peers ) {
		my $peerId = $peer->id();
		my $currentPower = $peer->power();
		
		# Only change power state if it's different
		if ( $currentPower != $powerState ) {
			$log->info("changing power state for $peerId from $currentPower to $powerState");
			$peer->execute( [ 'power', $powerState ] );
		} else {
			$log->debug("peer $peerId already has power state $powerState, skipping");
		}
	}
}

sub settingsManager_register {
	my $smApi = $Plugins::SettingsManager::Plugin::apiVersion;
	if ( defined($smApi) ) {
		# call setPrefName(), doNotCopy(), and registerPreCopyFilter(), here
		Plugins::SettingsManager::Public::doNotCopy('plugin.SyncOptions','pleaseRestore');
	}
}

sub shutdownPlugin {
	$log->debug("SyncOptions shutting down");
	# we should not act
	$pluginEnabled = 0;
	$log->debug("SyncOptions disabled\n");
}

sub enabled {
	if ( substr($::VERSION,0,3) gt '7.3' ) {
		$log->fatal("This plugin has been tested very little with SlimServer newer than 7.3.x");
	}
	return ($::VERSION ge '7.0');
}

sub vlVersion() {
	my $rcsVersion = '$Revision: 1.38 $';
	$rcsVersion =~ s/.*:\s*([0-9\.]*).*$/$1/;
	return $rcsVersion;
}

sub getMyDisplayName() {
	return 'PLUGIN_SYNCOPTIONS';
}

sub alarmSounding {
	my $request = shift;
	$log->debug("alarmSounding callback running");
	my $client = $request->client();
	if ( defined($client) ) {
		my $id = $client->id();
		&removeFromWaitingPlayers($id);
		$prefs->client($client)->set('pleaseRestore',0);
	}
}

sub newSong {
	my $request = shift;
	$log->debug("newSong callback running");
	my $client = $request->client();
	if ( defined($client) ) {
		my $id = $client->id();
		my $name = $client->name();
		my $thisSong = &getSongId($client);
		$log->debug("newSong: $id/$name playing $thisSong");
		if ( defined($id) && ($thisSong ne 'NONE')) {
			my $secsRemaining = &secondsLeft($client);
			$lastSongChange{$id} = Time::HiRes::time();
			$log->info( "new song on $id, updating lastSongChange to now" );
			# if players waiting for a new song on $id, set timer to join them
			if ( defined($waitingPlayers{$id}) && (!defined($timerSet{$id})) && (scalar(@{$waitingPlayers{$id}}) > 0) ) {
				$log->info( "setting timer for addJoiners to fire in $secsRemaining seconds");
				Slim::Utils::Timers::setTimer($client,&getPrecision() + Time::HiRes::time() + $secsRemaining,\&addJoiners,$id);
				$timerSet{$id} = 1;
			}
		}
	}
}

sub addJoiners {
	my ($client,$id) = @_;
	my $currentSong = &getSongId($client);
	# if $currentSong eq 'NONE', do nothing
	if ( $currentSong eq 'NONE' ) {
		return;
	}
	# or possibly clear the queue of joiners?
	if ( defined($lastSong{$id}) && ($currentSong eq $lastSong{$id}) ) {
		# too soon
		$log->debug( "addJoiners() fired too soon on $id, try again shortly");
		Slim::Utils::Timers::setTimer($client,Time::HiRes::time() + &getPrecision(),\&addJoiners,$id);
		return;
	}
	# join waiting clients
	&savePlaylistIfAlone($client);
	foreach my $joinerId ( @{$waitingPlayers{$id}} ) {
		my $co = &getClientObject($joinerId);
		if ( defined($co) ) {
			$log->warn( "addJoiners: $joinerId will now join sync group for $id");
			# BUG: use $originalSyncCommand
			&savePlaylistRegardless($co);
			if ( $isNewStreaming ) {
				$client->controller()->sync($co);
				#$client->execute( [ 'play' ] );
			} else {
				Slim::Player::Sync::sync($co,$client);
			}
			# if $joinerId in our special mode, pop it up
			if ( &isInOurMode($joinerId) ) {
				$inOurMode{$joinerId} = undef;
				Slim::Buttons::Common::popModeRight($co);
			}
			# clear some vars
			$waitingForName{$joinerId} = undef;
			$waitingForId{$joinerId} = undef;
			$forceSync{$joinerId} = undef;
		}
	}
	# clear list of waiting players
	$waitingPlayers{$id} = [ ];
	# reset song
	$lastSong{$id} = undef;
	# clear timerSet flag
	$timerSet{$id} = undef;
}

sub secondsLeft($) {
        my $client = shift;
        my $id = $client->id();
        # from Slim::Player::Source
	if ( $isNewStreaming ) {
		return (Slim::Player::Source::playingSongDuration($client) - Slim::Player::Source::songTime($client));	
	} 
       	my $rate = $client->rate();
        if (Slim::Player::Source::playingSongIndex($client) == Slim::Player::Source::streamingSongIndex($client)) {
                my $outputBufferFullness = $client->outputBufferFullness();
                if (defined($outputBufferFullness)) {
                        # Assume 44.1KHz output sample rate. This will be slightly
                        # off for anything that's 48Khz, but it's a guesstimate anyway.
                        return (($outputBufferFullness / (44100 * 8)) * $rate);
                }
        }
        return 0;
}

sub getClientObject($) {
	my $id = shift;
	my @players = Slim::Player::Client::clients();
	foreach my $client ( @players ) {
		if ( defined($client->id()) && ($id eq $client->id()) ) {
			return $client;
		}
	}
	$log->error( "no player found with id \"$id\"");
	return undef;
}

sub cancelSync($) {
	my $indexid = shift;
	if ( defined($waitingForName{$indexid}) ) {
		$waitingForName{$indexid} = undef;
		$waitingForId{$indexid} = undef;
		$forceSync{$indexid} = undef;
		&removeFromWaitingPlayers($indexid);
	}
}

# our sync wrapper function
sub SyncOptions_syncCommand {
	my @args = @_;
	$log->info("SyncOptions_syncCommand running");
	my $request = $args[0];
	my $client = $request->client();
	my $id = $client->id();
	my $indexid = $request->getParam('_indexid-');
	# in 7.3+, JUMP_IN (2) is treated like DEFAULT (0) -- &getWait() will *never* return 2
	my $waitPref = &getWait();
	if ( $pluginEnabled && defined($client) && ($indexid ne '-') && ( $waitPref != 0) ) {
		my $joiner = &getClientObject($indexid);
		$log->debug( "$indexid wants to sync to someone but might need to wait");
		# if group already playing and playing a file:// address
		# cleanup if already waiting to join a sync group
		# TODO: for mid-song, Pandora should be treated like local files
		&cancelSync($indexid);
		if ( defined($client) && (Slim::Player::Source::playmode($client) eq 'play') && (&getSongId($client) =~ m/FILE$/) ) {
			$log->debug( "$id is playing a FILE stream; see if it's soon enough to allow joining");
			# if we have a lastSongChange for target
			if ( defined($lastSongChange{$id}) ) {
				# if too late 
				my $timePassed = (Time::HiRes::time() - $lastSongChange{$id});
				if ( $timePassed > &getDelay ) {
					# avoid jumping in if not a natively supported audio format?
					if ( ($waitPref == 2) && &isJumpableSong($client,$joiner) ) {
						$log->info( "$id started FILE a while ago and jumpIn preferred; jumping in");
						# if in a older version of SC (waitPref never 2 in 7.3+) ...
						&jumpIn($joiner,$indexid,$client,$id);
						return;
					# if we prefer to jump but it's not jumpable, use the wait behavior
					} else {
						$log->info( "$id started FILE stream too long ago ($timePassed secs); $indexid will wait for next song on $id");
						&initiateWait($joiner,$indexid,$client,$id);
					}
					# return (do not run orig cmd)
					return;
				}
				$log->info( "$id started FILE recently; $indexid will sync now");
			}
		}
		&savePlaylistIfAlone($client);
		&savePlaylistRegardless($joiner);
	} elsif ( $pluginEnabled && defined($client) && ($indexid ne '-') && ( $waitPref == 0) ) {
		my $joiner = &getClientObject($indexid);
		$log->info( "save playlist(s) if restoring after unsync");
		&savePlaylistIfAlone($client);
		&savePlaylistRegardless($joiner);
		&setSyncVolumeTimer($indexid,$joiner);
	} elsif (defined($client) && ($indexid eq '-') ) {
		# remove $id from @{$waitingPlayers{$otherId}} so it's not joined late
		# vol sync forget
		&forgetVolume($client);
		&cancelSync($id);
	}
	# sync
	$log->info( "$id - $indexid sync command firing");
	my $rc = &$originalSyncCommand(@args);
	if (defined($client) && ($indexid eq '-') ) {
		$log->info( "restoring playlist if restoring after unsync");
		&restorePlaylist($client);
	}
	return $rc;
}

sub isJumpableSong($$) {
	my ($master,$joiner)= @_;
	if ( $isNewStreaming ) {
		# all is jumpable in 7.3
		return 1;
	}
	# TODO: Pandora is not jumpable
	my $url = Slim::Player::Playlist::url( $master, Slim::Player::Source::streamingSongIndex($master) );
	my $track = Slim::Schema->rs('Track')->objectForUrl($url);
	# all players can jump in to MP3
	if ( Slim::Music::Info::isMP3($track->url) ) { return 1; }
	# Squeezebox1 and SliMP3 do not natively support FLAC or WMA
	# the joiner:
	if ( $joiner->isa( "Slim::Player::SLIMP3") ) { return 0; }
	if ( $joiner->isa("Slim::Player::Squeezebox") && (!$joiner->isa("Slim::Player::Squeezebox2")) && (!$joiner->isa("Slim::Player::Transporter")) ) { return 0; }
	# the master:
	if ( $master->isa( "Slim::Player::SLIMP3") ) { return 0; }
	if ( $master->isa("Slim::Player::Squeezebox") && (!$master->isa("Slim::Player::Squeezebox2")) && (!$master->isa("Slim::Player::Transporter")) ) { return 0; }
	# the other sync group members:
	my @slaves;
	if ( $isNewStreaming ) {
		@slaves = Slim::Player::Sync::slaves($master);
	} else {
		@slaves = @{$master->slaves};
	}
	foreach my $client (@slaves) {
		if ($joiner->id() ne $client->id()) {
			if ( $client->isa( "Slim::Player::SLIMP3") ) { return 0; }
			if ( $client->isa("Slim::Player::Squeezebox") && (!$client->isa("Slim::Player::Squeezebox2")) && (!$client->isa("Slim::Player::Transporter")) ) { return 0; }
		}
	}
	# ok to sync mid-song with FLAC:
	if ( Slim::Music::Info::isFLAC($track->url) ) { return 1; }
	# WMA should work but doesn't on SB3 at firmware 86
	#if ( Slim::Music::Info::isType($track->url,'wma') ) { return 1; } 
	# SB3 cannot even jump in on Ogg Vorbis, so assume other formats won't work
	return 0;	# what we should return for unknown formats
}

sub initiateWait($$$$) {
	my ($joiner,$joinerid,$master,$masterid) = @_;
	push @{$waitingPlayers{$masterid}}, $joinerid;
	# note who we're waiting on
	$waitingForName{$joinerid} = $master->name();
	$waitingForId{$joinerid} = $masterid;
	$forceSync{$joinerid} = undef;
	my $thisSong = &getSongId($master);
	$lastSong{$masterid} = $thisSong;
	# push $joinerid into a mode saying it's waiting for the next song
	Slim::Buttons::Common::pushMode($joiner, $modeString);
}

sub removeFromWaitingPlayers($) {
	my $joinerid = shift;
	my @players = Slim::Player::Client::clients();
	foreach my $c ( @players ) {
		my $thisId = $c->id();
		my @waitList;
		foreach my $i ( @{$waitingPlayers{$thisId}} ) {
			if ( $i ne $joinerid ) { push @waitList, $i; }
		}
		$waitingPlayers{$thisId} = \@waitList;
		if ( scalar(@waitList) == 0 ) {
			# no need for a timer
			Slim::Utils::Timers::killTimers($c, \&addJoiners);
			$timerSet{$thisId} = undef;
		}
	}
}

# getsongid.txt
# verify that this notices updated metadata in streams
sub getSongId($) {
	my $client = shift;
	my $id = $client->id();
	my $song = Slim::Player::Playlist::song($client);
	if (! defined($song) ) { return 'NONE'; }
	# return several pieces of info, so it's more likely to recognize
	# changes if playing several recordings of the same song in a row
	# The following is good for local recordings
	my %data;
	$data{'album'} = defined($song->album()) ? ( ref $song->album() ? $song->album()->name() : $song->album() ) : '';
	$data{'artist'} = defined($song->artist()) ? ( ref $song->artist() ? $song->artist()->name(): $song->artist() ) : '';
	$data{'title'} = $song->name() || $song->title;
	if ( $song->path() =~ m|^[a-z0-9]{2,}:|i ) {
		my $url = Slim::Player::Playlist::url( $client, Slim::Player::Source::streamingSongIndex($client) );
		# try to get good metadata
		my $handler = Slim::Player::ProtocolHandlers->handlerForURL($url);
		if ( $handler && $handler->can('getMetadataFor') ) {
			my $metadata = $handler->getMetadataFor( $client, $url );
			if ( defined $metadata ) {
				foreach my $i ('artist', 'album', 'title', 'duration', 'info_link') {
					if (defined($metadata->{$i}) && ($metadata->{$i} =~ m/\S/) ) {
						$data{$i} = $metadata->{$i};
					}
				}
			}
		}
	}
	my $songinfo = join("\t",$data{'title'},$data{'album'},$data{'artist'},$song->duration(),$song->path()); 
	# but we need this to get the title from stream metadata
	# (only add this for streams; for local recordings, the following changes
	#  before the music really starts playing[!])
	if ( $song->path() =~ m|^[a-z0-9]{2,}:|i ) {
		my $url = Slim::Player::Playlist::url( $client, Slim::Player::Source::streamingSongIndex($client) );
		$songinfo .= "\t".Slim::Music::Info::getCurrentTitle(undef, $url)."\tURL";
	} else {
		$songinfo .= "\tFILE";
	}
	$log->debug("$id: currently playing \"$songinfo\"");
	return $songinfo;
}

sub isSynced($) {
	my $client = shift;
	if ( $isNewStreaming ) {
		if ( Slim::Player::Sync::isSlave($client) || Slim::Player::Sync::isMaster($client) ) {
			return 1;
		}
		return 0;
	}
	if ( Slim::Player::Sync::isSynced($client) ) { return 1; }
	return 0;
}

# our power wrapper function
sub SyncOptions_powerCommand {
	my @args = @_;
	my $request = $args[0];
	my $client = $request->client();
	$log->info("SyncOptions_powerCommand running for ".$client->name());
	my $newvalue = $request->getParam('_newvalue');
	# TODO: if $newvalue == $client->power(), nothing to do, bail after running originalPowerCommand
	my $unsync = 0;
	my $powerSync = 0;
	
	# Check if we should unsync at power off
	if ( $pluginEnabled && defined($client) && (&getUnsync == 1) &&  ($newvalue == 0) && &isSynced($client) ) {
		$unsync = 1;
	}
	
	# Check if we should sync power state across sync group
	if ( $pluginEnabled && defined($client) && (&getPowerSync == 1) && &isSynced($client) ) {
		$powerSync = 1;
	}
	
	$log->debug( "original power command");
	my $rc = &$originalPowerCommand(@args);
	
	if (! $client->power ) {
		# remove $client from any pre-join lists it's in
		&removeFromWaitingPlayers($client->id());
	}
	
	# Handle power synchronization across sync group
	if ( $powerSync == 1 ) {
		$log->info( "syncing power state across sync group");
		&syncPowerAcrossGroup($client, $newvalue);
	}
	
	if ( $unsync == 1 ) {
		# BUG: use $originalSyncCommand
		$log->info( "unsync at power off");
		&unsync($client);
		# restoring the playlist would power us back on; don't do that
		#&restorePlaylist($client);
		# forget about restoring playlist; should help with alarms
		$prefs->client($client)->set('pleaseRestore',0);
	} else {
		if ( $pluginEnabled && defined($client) && ($newvalue == 1) && (!&isSynced($client)) && ($prefs->client($client)->get('pleaseRestore') == 1) ) {
			# would be nice to restore playlist if certain this wasn't an alarm sounding
		}
	}
	return $rc;
}

sub getConfig {
	my $unsync = $prefs->get('unsync');
	if (! defined($unsync) ) {
		$unsync = $defaultUnsyncAtPowerOff;
		$prefs->set('unsync',$unsync);
        }
	my $wait = $prefs->get('wait');
	if (! defined($wait) ) {
		$wait = $defaultWaitToSync;
		$prefs->set('wait',$wait);
        }
	my $delay = $prefs->get('delay');
	if (! defined($delay) ) {
		$delay = $defaultSyncDelayMax;
		$prefs->set('delay',$delay);
        }
	my $precision = $prefs->get('precision');
	if (! defined($precision) ) {
		$precision = $defaultPrecisionSecs;
		$prefs->set('precision',$precision);
        }
	my $volSync = $prefs->get('volumeSync');
	if (! defined($volSync) ) {
		$volSync = $defaultVolumeSync;
		$prefs->set('volumeSync',$volSync);
        }
	my $volSyncDelay = $prefs->get('volumeSyncDelay');
	if (! defined($volSyncDelay) ) {
		$volSyncDelay = $defaultVolumeSyncDelay;
		$prefs->set('volumeSyncDelay',$volSyncDelay);
        }
	my $powerSync = $prefs->get('powerSync');
	if (! defined($powerSync) ) {
		$powerSync = $defaultPowerSync;
		$prefs->set('powerSync',$powerSync);
        }
	return ($unsync, $wait, $delay, $precision, $volSync, $volSyncDelay, $powerSync);
}	

# function that fires when a user updates their preferences
sub prefUpdate {
	my $client = shift;
	$log->info("prefUpdate running");
}

# Web preferences code:
sub validateDelay {
	my $delay = shift;
	if ( ($delay =~ m/^[0-9\.]*$/) && ($delay !~ m/\..*\./) ) {
		return $delay;
	}
	return undef;
}

sub validateWait {
	my $wait = shift;
	if ( $wait =! m/^[012]$/ ) {
		return $wait;
	}
	return undef;
}

sub getUnsync {
	my $client = shift;
	my ($unsync,$wait,$delay,$precision,$volSync,$volSyncDelay,$powerSync) = &getConfig($client);
	return $unsync;
}

sub getPowerSync {
	my $client = shift;
	my ($unsync,$wait,$delay,$precision,$volSync,$volSyncDelay,$powerSync) = &getConfig($client);
	return $powerSync;
}

sub getWait {
	my $client = shift;
	my ($unsync,$wait,$delay,$precision,$volSync,$volSyncDelay,$powerSync) = &getConfig($client);
	# in 7.3+, JUMP_IN (2) is treated like DEFAULT (0)
	if ( ($wait == 2) && $isNewStreaming ) { return 0; }
	return $wait;
}

sub getDelay {
	my $client = shift;
	my ($unsync,$wait,$delay,$precision,$volSync,$volSyncDelay,$powerSync) = &getConfig($client);
	return $delay;
}

sub getPrecision {
	my $client = shift;
	my ($unsync,$wait,$delay,$precision,$volSync,$volSyncDelay,$powerSync) = &getConfig($client);
	return $precision;
}
		
# required function to indicate what settings to offer in the web UI
sub setupGroup {
	my $client = shift;
   
	my %setupGroup = (
		PrefOrder => ['unsync', 'powerSync', 'wait', 'delay', 'precision'],
			,GroupHead => Slim::Utils::Strings::string('SETUP_GROUP_PLUGIN_SYNCOPTIONS')
			,GroupLine => 1
			,GroupSub => 1
			,Suppress_PrefSub => 1
			,Suppress_PrefLine => 1
			,Suppress_PrefHead => 1
			,Suppress_PrefDesc => 0
	);

	my %setupPrefs = (
		'unsync' => {
			'validate' => \&Slim::Utils::Validate::trueFalse
			,PrefDesc => Slim::Utils::Strings::string('SETUP_PLUGIN-SYNCOPTIONS_UNSYNC')
			,'onChange' => \&Plugins::SyncOptions::Plugin::prefUpdate
			,currentValue => \&Plugins::SyncOptions::Plugin::getUnsync
			,'options' => {
					'1' => 'MCON',
					'0' => 'MCOFF',
					},
		},
		'powerSync' => {
			'validate' => \&Slim::Utils::Validate::trueFalse
			,PrefDesc => Slim::Utils::Strings::string('SETUP_PLUGIN-SYNCOPTIONS_POWERSYNC')
			,'onChange' => \&Plugins::SyncOptions::Plugin::prefUpdate
			,currentValue => \&Plugins::SyncOptions::Plugin::getPowerSync
			,'options' => {
					'1' => 'MCON',
					'0' => 'MCOFF',
					},
		},
		'wait' => {
			PrefDesc => Slim::Utils::Strings::string('SETUP_PLUGIN-SYNCOPTIONS_WAIT')
			,'onChange' => \&Plugins::SyncOptions::Plugin::prefUpdate
			# in 7.3+, JUMP_IN (2) is treated like DEFAULT (0) -- &getWait() will *never* return 2
			,currentValue => \&Plugins::SyncOptions::Plugin::getWait
			,'options' => {
					'0' => 'DEFAULT_SKIN',
					'1' => 'PLUGIN_SYNCOPTIONS_WAIT',
					'2' => 'PLUGIN_SYNCOPTIONS_JUMPIN',
					},
		},
		'delay' => {
			'validate' => \&Plugins::SyncOptions::Plugin::validateDelay
			,currentValue => \&Plugins::SyncOptions::Plugin::getDelay
			,PrefDesc => Slim::Utils::Strings::string('SETUP_PLUGIN-SYNCOPTIONS_DELAY')
		},
		'precision' => {
			'validate' => \&Plugins::SyncOptions::Plugin::validateDelay
			,currentValue => \&Plugins::SyncOptions::Plugin::getPrecision
			,PrefDesc => Slim::Utils::Strings::string('SETUP_PLUGIN-SYNCOPTIONS_PRECISION')
		},
	);	

	# the final "1" arg says to put this in the player prefs   
	return (\%setupGroup,\%setupPrefs,0);
}

# BUG: should really look at mode stack?
sub isInOurMode($) {
	my $id = shift;
	if ( defined($inOurMode{$id}) ) { return 1; }
	return 0;
}

sub unsetInitialSyncVolume {
	my ($client) = @_;
	my $id = $client->id();
	#my $syncController = $client->controller;
	#my $master = $syncController->master();
	#my $cv = $serverPrefs->client($client)->get('volume');
	#my $mv = $serverPrefs->client($master)->get('volume');
	#my $ratio = $cv / $mv;
	$log->debug("forgetting volume for $id since no longer synced");
	$initialSyncVolume{$id} = undef;
}

sub setInitialSyncVolume {
	my ($client,$show) = @_;
	my $id = $client->id();
	my $vol = $serverPrefs->client($client)->get('volume');
	if ( $vol < 1 ) { $vol = 1; }	# prevent division by zero
	$initialSyncVolume{$id} = $vol;
	$log->debug("note that $id currently as volume $vol");
	if ( $prefs->get('volumeSync') == 1 ) {
	# 	if using relative volume sync
	#	 note (showBriefly) that volume will now be synced
		if ( $show ) {
			$client->showBriefly( { 'line' => [ string(&getMyDisplayName()),string('PLUGIN_SYNCOPTIONS_RELATIVE_SYNC_NOW')]},{'duration' => 2, 'block' => 1, } );
		}
	}
}

sub setSyncVolumeTimer($$) {
	my ($id,$client) = @_;
	Slim::Utils::Timers::killTimers($client, \&setInitialSyncVolume);
	my $vdelay = $prefs->get('volumeSyncDelay');
	$log->debug("will note volume of $id in $vdelay seconds");
	Slim::Utils::Timers::setTimer($client, (Time::HiRes::time() + $vdelay), \&setInitialSyncVolume, 1);
}

sub jumpIn($$$$) {
	my ($client,$id,$master,$masterId) = @_;
	$log->info( "jumpIn firing to sync $id with $masterId");
	&savePlaylistIfAlone($master);
	&savePlaylistRegardless($client);
	# set timer to calculate relative volume for volume syncing
	&setSyncVolumeTimer($id,$client);
	if ( $isNewStreaming ) {
		$log->debug( "jumpIn syncing");
		$master->execute( [ 'pause' ] );
		$master->controller()->sync($client);
		$master->execute( [ 'pause' ] );
		# done!
		return;
	}
	# 
	# The following is for non-new-streaming SC instances
	# this technique is from kdf's Song Scanner
	$client->showBriefly( { 'line' => [ string(&getMyDisplayName()),string('PLUGIN_SYNCOPTIONS_SYNCING_WITH').$waitingForName{$id}."..."]},{'duration' => 1, 'block' => 1, } );
	$log->debug( "jumpIn getting master offset/position");
	my $masterSong = &getSongId($master);
	my $masterPos = Slim::Player::Source::songTime($master);
	$log->debug( "jumpIn master is at playtime $masterPos");
	#$log->debug( "jumpIn pausing master");
	#Slim::Player::Source::playmode($master,"pause");
	# BUG: use $originalSyncCommand
	$log->debug( "jumpIn syncing");
	Slim::Player::Sync::sync($client,$master);
	$log->debug( "jumpIn setting playmode");
	Slim::Player::Source::playmode($master,"play");
	$log->debug( "jumpIn calling gototime");
	Slim::Player::Source::gototime($master, $masterPos, 1);
	# cleanup
	$log->debug( "jumpIn cleaning up");
	&cancelSync($id);
	$forceSync{$id} = undef;
	# did it work?
	my $masterPosNow = Slim::Player::Source::songTime($master);
	my $masterSongNow = &getSongId($master);
	if ( ($masterSongNow eq $masterSong) && ($masterPosNow < (0.9 * $masterPos)) ) {
		# probably a format we cannot sync into
		# so... too bad, we've already restarted from the top
	}
}

our %buttonFunctions = (
	'up' => sub  {
		my $client = shift;
		$client->bumpUp($client);
	},
	'down' => sub  {
	    my $client = shift;
		$client->bumpDown($client);
	},
	'right' => sub  {
		my $client = shift;
		my $id = $client->id();
		my $masterId = $waitingForId{$id};
		if ( defined($masterId) ) {
			# avoid jumping in if not a natively supported audio format
			my $master = &getClientObject($masterId);
			if ( &isJumpableSong($master,$client) ) {
				&jumpIn($client,$id,$master,$masterId);
				$inOurMode{$id} = undef;
				Slim::Buttons::Common::popModeRight($client);
				return;
			}
		}
		$whichString{$id} = 'PLUGIN_SYNCOPTIONS_PLAY_TO_SYNC_NOJUMP';
		$client->bumpRight($client);
	},
	'left' => sub  {
		my $client = shift;
		my $id = $client->id();
		$client->showBriefly( { 'line' => [string(&getMyDisplayName()),string('PLUGIN_SYNCOPTIONS_CANCEL') ]},{'duration' => 1, 'block' => 1, } );
		&cancelSync($id);
		# also deliberately unsync this player 
		# (in my testing, my kitchen player would get all kinds of
		#  confused if extended microwave use would mess with its
		#  sync group connection; this lets a Left/Cancel undo that)
		&restorePlaylist($client);
		&unsync($client);
		$inOurMode{$id} = undef;
		Slim::Buttons::Common::popModeRight($client);
	},
	'play' => sub {
		my $client = shift;
		my $id = $client->id();
		# set $forceSync
		$forceSync{$id} = 1;
		# get ref to sync group master
		my $master = &getClientObject($waitingForId{$id});
		# showBriefly "syncing now"
		$client->showBriefly( { 'line' => [ string(&getMyDisplayName()),string('PLUGIN_SYNCOPTIONS_SYNCING_WITH').$waitingForName{$id}."..." ]},{'duration' => 1, 'block' => 1, } );
		# remove id from waitingPlayers array
		&removeFromWaitingPlayers($id);
		&savePlaylistIfAlone($master);
		&savePlaylistRegardless($client);
		if ( $isNewStreaming ) {
			$master->controller()->sync($client);
		} else {
			# stop song on sync group
			$master->execute( [ 'stop' ] );
			# exec sync command
			# BUG: use $originalSyncCommand
			Slim::Player::Sync::sync($client,$master);
			# start music again
			$master->execute( [ 'play' ] );
		}
		# unset $forceSync
		$forceSync{$id} = undef;
		# get out of this mode
		Slim::Buttons::Common::popModeRight($client);
	}
);

sub forgetVolume($) {
	my $client = shift;
	Slim::Utils::Timers::killTimers($client, \&setInitialSyncVolume);
	&unsetInitialSyncVolume($client);
}

sub unsync($) {
	my $client = shift;
	&forgetVolume($client);
	if ( $isNewStreaming ) {
		$client->controller()->unsync($client);
	} else {
		Slim::Player::Sync::unsync($client);
	}
}

sub getFunctions {
	return \%buttonFunctions;
}

sub lines {
	my $client = shift;
	my $id = $client->id();
	$inOurMode{$client->id()} = 1;
	my $waitName = string('UNK');
	if ( defined($waitingForName{$id}) ) { $waitName = $waitingForName{$id}; }
	my ($line1, $line2);
	$line1 = string($whichString{$id});
	$line2 = string('PLUGIN_SYNCOPTIONS_WAITING_FOR') . $waitName;
	return ( { 'line' => [$line1, $line2] } );
}

sub setWhichString($) {
	my $client = shift;
	my $id = $client->id();
	# default: offer force restart and cancel
	$whichString{$id} = 'PLUGIN_SYNCOPTIONS_PLAY_TO_SYNC_NOJUMP';
	my $masterId = $waitingForId{$id};
	if ( defined($masterId) ) {
		my $master = &getClientObject($masterId);
		if ( &isJumpableSong($master,$client) ) {
			# offer mid-song 
			$whichString{$id} = 'PLUGIN_SYNCOPTIONS_PLAY_TO_SYNC';
		}
	}
}

sub setWaitingMode() {
	my $client = shift;
	&setWhichString($client);
	$client->lines(\&lines);
}

sub restorePlaylist($) {
	my $client = shift;
	if ( $prefs->get('restoreAfterUnsync') != 1 ) {
		# we should not restore the playlist
		return;
	}
	if ( $prefs->client($client)->get('pleaseRestore') != 1 ) {
		# don't restore
		return;
	}
	if (! $client->power() ) {
		# restoring would wake the player up
		$prefs->client($client)->set('pleaseRestore',0);
		return;
	}
	# this loads the playlist and starts it
	my $name = &getPlaylistName($client);
	# bail out if no such playlist
	if ( &playlistExists($name) == 0 ) {
		$prefs->client($client)->set('pleaseRestore',0);
		return;
	}
	$log->is_debug && $log->debug( "restoring playlist \"$name\" for ".$client->id());
#print STDERR "restoring playlist \"$name\"\n";
	$client->execute(['playlist', 'resume', $name]);
	# but we likely don't want to play right away
	$client->execute(['stop']);
}

sub playlistExists($) {
	my $listname = shift;
	my $filename = catdir(preferences('server')->get('playlistdir'),"${listname}.m3u");
	if ( -e $filename ) {
		return 1;
	}
	return 0;
}

sub savePlaylistIfAlone($) {
	my $master = shift;
	if ( $isNewStreaming ) {
		my @slaves = Slim::Player::Sync::slaves($master);
		if ( scalar(@slaves) > 0 ) { return; }
	} else {
		if ( scalar(@{$master->slaves}) > 0 ) { return; }
	}
	$log->is_debug && $log->debug( "saving playlist for ".$master->id()." since it is not synced with any other players");
	&savePlaylistRegardless($master);
}
	
sub savePlaylistRegardless($) {
	my $client = shift;
	my $name = &getPlaylistName($client);
	if ( $prefs->get('restoreAfterUnsync') != 1 ) {
		$prefs->client($client)->set('pleaseRestore',0);
		#$log->debug( "deleting playlist \"$name\"");
#print STDERR "deleting playlist\n";
		# we should not restore the playlist, and might as well delete it if it exists
		#$client->execute(['playlist', 'delete', &getPlaylistName($client)]);
		return;
	}
#print STDERR "saving playlist \"$name\"\n";
	$log->is_debug && $log->debug( "saving playlist \"$name\" for ".$client->id());
	$client->execute(['playlist', 'save', $name]);
	$prefs->client($client)->set('pleaseRestore',1);
}

sub getPlaylistName {
	my $client = shift;
	return 'SyncOptions-'.&simplify($client->name()).'-'.&simplify($client->id());
}

sub simplify($) {
	my $text = shift;
	$text =~ s/[:\.]/_/g;
	$text =~ s/[^a-zA-Z0-9 _\-]//g;
	return $text;
}

# To offer a ContextMenu choice, a plugin must do three things:

# 1) register a pre-offer sub
#
# All registered subs are executed each time a context menu
# is created. This allows your plugin to indicate if it has
# any context options to offer.
#
 sub contextmenu_register {
 	my $contextMenuApi = $Plugins::ContextMenu::Plugin::apiVersion;
 	if ( defined($contextMenuApi) && ($contextMenuApi >= 0.67) ) {
 		Plugins::ContextMenu::Public::registerContextChoice( { 
		 	uid => 'plugin.SyncOptions.groupvolume', 	# REQUIRED. uniquely identify this top-level CM choice
 			coderef => \&contextmenu_preOffer, 	# REQUIRED. reference to your pre-offer function
 			api => 0.69,		# HIGHLY RECOMMENDED. the $contextMenuApi version supported by this plugin
 						# This allows CM to log warnings if the user has installed a newer
 						# version of CM than the one you developed for
 			displayname => string('PLUGIN_SYNCOPTIONS_GROUP_VOLUME'),
			pluginname => string('PLUGIN_SYNCOPTIONS'),	# REQUIRED. for the player UI
			enabled => 1,	# (optional) "1" to enable by default, "0" to require user to manually enable in settings
			autorun => 0,	# (optional) if set to "1", ContextMenu will automatically run the coderef you
						# supply if the user requests a context menu and this coderef is the only option.
						# 'autorun' is best for options that require further user confirmation. For instance,
						# if your CM option's coderef merely displays a list of options that the user can
						# choose from (or left-arrow out of), then 'autorun' might be good for it
			offerargs => {'label' => string('PLUGIN_SYNCOPTIONS_GROUP_VOLUME')}  
								# (optional) offerargs can hold whatever you need to pass
		} );
 	}
}

#
# This sub should be called by initPlugin, with code like this:
#   # register ContextMenu function after other plugins have had time to load
#   Slim::Utils::Timers::setTimer('moot', (Time::HiRes::time() + 1), \&contextmenu_register);


# 2) implement a pre-offer function
#
# $inputHashPtr has several elements: 
# 	client		(a $client object)
# 	id		($client->id(), saves a minor call)
# 	offerargs	(args element passed in the call to registerContextChoice)
# 	selected	(the item that was selected when ContextMenu was called)
# 	mode		(the mode the client was in when the context menu was requested)
# 	eventid		(unique identifier for a particular request for context menus;
# 			 if your plugin registers multiple options, you can use this
# 			 as a key for saving information for use by your coderefs)
# 	autorun		(as of API 0.69) -- "1" if an autorun invocation, "0" if normal UI selection
#
# This sub should return a hash with the following elements:
# 	label		(text to display on the player [NOT a string token, but actual text])
# 	coderef		(a codref pointer to code to be executed when {label} is selected;
# 			 return undef if your plugin has no options to offer)
# 	cleanupcode	[OPTIONAL] (coderef to be called after all pre-offer coderefs have been run
# 			 for a particular eventid; set *one* of these if you're using multiple
# 			 pre-offer coderefs. This coderef will be passed one arg, the eventid 
# 			 passed to the pre-offer coderefs)
# 	execargs	(passed verbatim to coderef if option chosen; a handy place to stash info you want carried over)
#
sub contextmenu_preOffer {
	my $inputHashPtr = shift;
	my $coderef = \&contextmenu_mainFunction;
	my $client = $inputHashPtr->{client};
	# here we could check the status of the player ($inputHashPtr->{client}),
	# the currently selected item ($inputHashPtr->{selected}), etc.
	# and set $coderef to undef if we did not have any context option to offer
	# only if we're synced
	my $label = string('PLUGIN_SYNCOPTIONS_ADJUST_GROUP_VOLUME');
	if (! $client->isSynced() || $serverPrefs->client($client)->get('syncVolume')) {
#print STDERR "don't offer CM, either not synced or core volume sync enabled)\n";
		$coderef = undef;
	} else {
		my $volSync = $prefs->get('volumeSync');
		if ( $volSync == 1 ) {
			$label = string('PLUGIN_SYNCOPTIONS_ADJUST_PLAYER_VOLUME');
		}
	}
	return ( { label => $label, coderef => $coderef, execargs => $inputHashPtr->{offerargs} } );
}


# 3) implement functions that your pre-offer function refers to
#    in the {coderef} element of the hash array it returns
#
# $inputHashPtr has several elements: 
# 	client		(a $client object)
# 	execargs	(args element passed by the pre-offer function)
# 	method		(what button was pressed: 'add', 'play', or 'right')
#
# This function can execute code, push the $client into a new mode with more options, whatever.
# CM makes no assumptions about what this routine will do, or how quickly it will finish.
#
sub contextmenu_mainFunction {
 	my $inputHashPtr = shift;
 	my $client = $inputHashPtr->{client};
#print STDERR "SO CM command running\n";
	# push into our custom mode
	Slim::Buttons::Common::pushModeLeft($client, 'PLUGIN.SyncOptions.VolumeSyncOverride');
}

sub setMode {
        my $client = shift;
        my $method = shift;
 	my $id = $client->id();
#print STDERR "setting SO CM mode\n";

        if ($method eq 'pop') {
		my @mstack = @{$client->modeStack};
#print STDERR join(', ',@mstack)."\n";
#print STDERR "cancel override\n";
		# unset override
		$volumeSyncOverride{$id} = undef;
		# clear out our display
 		Slim::Buttons::Common::popModeRight($client);
 		# we pop out of CM mode so we don't see CM options again
 		Slim::Buttons::Common::popModeRight($client);
 		# and indicate that we're done (would do this when your code is done -- e.g., if you
 		# push to INPUT.List, you would not call finish() until the user had finished working
 		# with your menu)
 		if ( $Plugins::ContextMenu::Plugin::apiVersion >= 0.68 ) { Plugins::ContextMenu::Public::finish($client); }
		return;
	}

	# set override
#print STDERR "set override\n";
	my $volSync = $prefs->get('volumeSync');
	if ( $volSync == 0 ) { $volumeSyncOverride{$id} = 1; }
	else { $volumeSyncOverride{$id} = 0; }
	# display info
	$client->lines(\&lines);
}

sub lines {
	my $client = shift;
 	my $id = $client->id();
	my ($line1, $line2);
	$line1 = string('PLUGIN_SYNCOPTIONS');
	if ( $volumeSyncOverride{$id} == 0 ) {
		$line2 = string('PLUGIN_SYNCOPTIONS_ADJUST_PLAYER_VOLUME_NOW');
	} else {
		$line2 = string('PLUGIN_SYNCOPTIONS_ADJUST_GROUP_VOLUME_NOW');
	}
	return ( { 'line' => [$line1, $line2] } );
}

our %configFunctions = (
#	'left' => sub  {
#		my $client = shift;
# 		my $id = $client->id();
#		Slim::Buttons::Common::popModeRight($client);
#	},
); 

sub getFunctions {
        return \%configFunctions;
}

1;

