# SyncOptions LMS Plugin

Enhanced synchronization options for Logitech Media Server (LMS) players, including power state synchronization.

## Features

- **Power Synchronization**: Turn on/off all players in a sync group together
- **Unsync at Power Off**: Automatically unsync players when powered off
- **Relative Volume Sync**: Proportional volume changes across synced players
- **Wait for Next Track**: Avoid disruptions when joining sync groups
- **Restore Playlist**: Restore local playlist after leaving sync group

## Repository Structure

```
SyncOptions/
├── lib/                    # Perl source code
│   └── Plugins/
│       └── SyncOptions/
│           ├── Plugin.pm      # Main plugin logic
│           ├── Settings.pm    # Web interface settings
│           └── install.xml    # Plugin metadata
├── share/                  # Static assets
│   └── HTML/              # Web interface templates
│       └── EN/
│           └── plugins/
│               └── SyncOptions/
│                   ├── html/
│                   │   └── donate.html
│                   └── settings/
│                       └── basic.html
├── t/                      # Tests (future)
├── docs/                   # Distribution artifacts (GitHub Pages)
│   ├── repodata.xml       # LMS repository metadata
│   └── SyncOptions/       # Plugin ZIP files
│       └── SyncOptions-2.3.0.zip
├── build.py               # Build script
├── Makefile              # Build automation
├── strings.txt           # Localization strings
├── COPYRIGHT.txt         # License
└── README.md            # This file
```

## Building

### Requirements
- Python 3.6+
- Make (optional)

### Build Commands

```bash
# Build the plugin
python3 build.py

# Or using Make
make build

# Clean build artifacts
make clean

# Test the build
make test

# Show repository structure
make structure
```

### What the Build Does

1. **Collects source files** from `lib/` and `share/`
2. **Creates plugin structure** in temporary `build/` directory
3. **Builds ZIP file** with proper path separators (forward slashes)
4. **Calculates SHA1 hash** for integrity verification
5. **Updates repository XML** with new hash
6. **Cleans up** temporary files

## Installation

### Method 1: Repository Installation (Recommended)
1. Add repository URL to LMS: `https://kitschen.github.io/syncoptions-repo/docs/repodata.xml`
2. Install "SyncOptions" from the plugin list

### Method 2: Manual Installation
1. Download `SyncOptions-2.3.0.zip` from the repository
2. Extract to LMS plugins directory
3. Restart LMS

## Usage

1. **Enable Power Sync**: Go to Settings → Plugins → SyncOptions
2. **Turn on "Power synchronization"**
3. **Create a sync group** with multiple players
4. **Test**: Power on/off any player in the group

## Development

### Adding Features
1. Edit source files in `lib/Plugins/SyncOptions/`
2. Update version in `build.py` and `install.xml`
3. Run `python3 build.py` to rebuild
4. Test the new ZIP file

### Repository Deployment
The `docs/` directory contains the LMS repository structure for GitHub Pages:
- Upload repository to GitHub 
- Configure GitHub Pages to publish from `/docs` folder
- LMS will read from `https://kitschen.github.io/syncoptions-repo/docs/repodata.xml`

## License

GPL v2 - See COPYRIGHT.txt for details

## Credits

- Original plugin by Peter Watkins
- Power synchronization feature added in v2.3.0