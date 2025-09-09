#!/usr/bin/env python3
"""
Build script for SyncOptions LMS Plugin

This script:
1. Collects source files from lib/ and share/
2. Creates the proper plugin directory structure
3. Builds a ZIP file with correct path separators
4. Calculates SHA1 hash
5. Updates the repository XML file
"""

import os
import shutil
import zipfile
import hashlib
import xml.etree.ElementTree as ET
from pathlib import Path

# Configuration
PLUGIN_NAME = "SyncOptions"
VERSION = "2.3.0"
BUILD_DIR = "build"
DIST_DIR = "dist/SyncOptions"
ZIP_FILE = f"{DIST_DIR}/{PLUGIN_NAME}-{VERSION}.zip"
REPO_XML = "dist/repodata.xml"
GITHUB_REPO_URL = "https://kitschen.github.io/syncoptions-repo/dist" 

def clean_build():
    """Remove build directory if it exists"""
    if os.path.exists(BUILD_DIR):
        shutil.rmtree(BUILD_DIR)
    print("✓ Cleaned build directory")

def create_plugin_structure():
    """Create the plugin directory structure in build/"""
    plugin_dir = f"{BUILD_DIR}/{PLUGIN_NAME}"
    os.makedirs(plugin_dir, exist_ok=True)
    
    # Copy Perl modules
    for file in ["Plugin.pm", "Settings.pm", "install.xml"]:
        src = f"lib/Plugins/SyncOptions/{file}"
        if os.path.exists(src):
            shutil.copy(src, plugin_dir)
            print(f"✓ Copied {file}")
    
    # Copy HTML templates
    if os.path.exists("share/HTML"):
        shutil.copytree("share/HTML", f"{plugin_dir}/HTML")
        print("✓ Copied HTML templates")
    
    # Copy txt files
    if os.path.exists("share/txt"):
        shutil.copytree("share/txt", f"{plugin_dir}")
        print("✓ Copied txt files")


def create_zip():
    """Create ZIP file with proper path separators"""
    os.makedirs(os.path.dirname(ZIP_FILE), exist_ok=True)
    
    with zipfile.ZipFile(ZIP_FILE, 'w', zipfile.ZIP_DEFLATED) as zf:
        for root, dirs, files in os.walk(BUILD_DIR):
            for file in files:
                file_path = os.path.join(root, file)
                # Create archive name with forward slashes and remove 'build/' prefix
                arcname = file_path.replace(BUILD_DIR + os.sep, '').replace(os.sep, '/')
                zf.write(file_path, arcname)
                
    print(f"✓ Created ZIP: {ZIP_FILE}")

def calculate_sha1():
    """Calculate SHA1 hash of the ZIP file"""
    with open(ZIP_FILE, 'rb') as f:
        sha1_hash = hashlib.sha1(f.read()).hexdigest()
    
    print(f"✓ SHA1: {sha1_hash}")
    return sha1_hash

def update_repository_xml(sha1_hash):
    """Update the repository XML with new SHA1 hash"""
    if not os.path.exists(REPO_XML):
        print(f"⚠ Repository XML not found: {REPO_XML}")
        return
    
    try:
        tree = ET.parse(REPO_XML)
        root = tree.getroot()
        
        # Find the plugin and update both SHA and URL
        for plugin in root.findall('.//plugin'):
            name_attr = plugin.get('name')
            version_attr = plugin.get('version')
            if name_attr == PLUGIN_NAME and version_attr == VERSION:
                # Update SHA1
                sha_elem = plugin.find('sha')
                if sha_elem is not None:
                    sha_elem.text = sha1_hash
                
                # Update URL to match new structure
                url_elem = plugin.find('url')
                if url_elem is not None:
                    expected_url = f"{GITHUB_REPO_URL}/{PLUGIN_NAME}/{PLUGIN_NAME}-{VERSION}.zip"
                    url_elem.text = expected_url
                    print(f"✓ Updated URL: {expected_url}")
                
                tree.write(REPO_XML, encoding='utf-8', xml_declaration=True)
                print(f"✓ Updated repository XML with SHA1: {sha1_hash}")
                return
        
        print(f"⚠ Plugin {PLUGIN_NAME} v{VERSION} not found in repository XML")
        
    except ET.ParseError as e:
        print(f"✗ Error parsing repository XML: {e}")

def cleanup():
    """Remove build directory"""
    if os.path.exists(BUILD_DIR):
        shutil.rmtree(BUILD_DIR)
    print("✓ Cleaned up build directory")

def main():
    """Main build process"""
    print(f"Building {PLUGIN_NAME} v{VERSION}")
    print("=" * 40)
    
    try:
        clean_build()
        create_plugin_structure()
        create_zip()
        sha1_hash = calculate_sha1()
        update_repository_xml(sha1_hash)
        cleanup()
        
        print("=" * 40)
        print("✅ Build completed successfully!")
        print(f"📦 Plugin ZIP: {ZIP_FILE}")
        print(f"🔗 SHA1: {sha1_hash}")
        
    except Exception as e:
        print(f"✗ Build failed: {e}")
        cleanup()
        return 1
    
    return 0

if __name__ == '__main__':
    exit(main())
