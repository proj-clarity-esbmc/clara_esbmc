#!/bin/bash

set -e
set -u
set -o pipefail

echo "Starting CLARA_ESBMC installation and environment setup..."

# Function to handle errors
error_exit() {
  echo "Error: $1" >&2
  exit 1
}

# Step 1: Ensure Xcode or Command Line Tools are installed
echo "Checking for Xcode installation..."
if ! xcodebuild -version &> /dev/null; then
  echo "Xcode not found. Installing Command Line Tools..."
  sudo xcode-select --install || error_exit "Failed to install Command Line Tools."
else
  echo "Xcode is already installed."
fi

# Step 2: Check if the correct version of Homebrew is installed
echo "Checking Homebrew installation..."
BREW_PATH=$(which brew || true)
if [[ "$BREW_PATH" != "/opt/homebrew/bin/brew" ]]; then
  error_exit "Homebrew is not installed in /opt/homebrew. Please ensure the Apple Silicon version of Homebrew is installed."
else
  echo "Homebrew is correctly installed."
fi

# Verify Homebrew config for Apple Silicon
if brew config | grep -q "Rosetta 2: false"; then
  echo "Running on Apple Silicon (arm64)."
else
  error_exit "Rosetta 2 is enabled, please disable it to proceed."
fi

# Step 3: Uninstall other versions of LLVM if necessary
echo "Checking for multiple versions of LLVM..."
if brew list | grep -q llvm; then
  INSTALLED_LLVM=$(brew list | grep llvm)
  if [[ "$INSTALLED_LLVM" != "llvm@15" ]]; then
    echo "Found other LLVM versions: $INSTALLED_LLVM. Uninstalling..."
    brew uninstall $INSTALLED_LLVM || error_exit "Failed to uninstall $INSTALLED_LLVM."
  else
    echo "Only llvm@15 is installed."
  fi
else
  echo "No LLVM versions found."
fi

# Step 4: Update Homebrew and Upgrade all packages
echo "Updating Homebrew and upgrading all packages..."
brew update && brew upgrade || error_exit "Failed to update and upgrade Homebrew."

# Step 5: Install dependencies via Homebrew
echo "Installing dependencies: gmp, cmake, boost, ninja, python3, wget, automake, flex, bison, z3, zstd, llvm@15..."
brew install gmp cmake boost ninja python3 wget automake flex bison z3 zstd llvm@15 || error_exit "Failed to install dependencies."

# Step 6: Set up paths
echo "Setting up LLVM paths..."
echo 'export PATH="/opt/homebrew/opt/llvm@15/bin:$PATH"' >> ~/.profile
export PATH="/opt/homebrew/opt/llvm@15/bin:$PATH"
echo "LLVM path set."

# Add zstd library to LIBRARY_PATH, initializing it if not set
echo "Adding zstd library to LIBRARY_PATH..."
export LIBRARY_PATH=${LIBRARY_PATH:-""}:$(brew --prefix zstd)/lib/
echo "zstd library path added."

# Reload the profile to apply changes
source ~/.profile || error_exit "Failed to reload profile."

# Step 7: Install PySMT
echo "Installing PySMT..."
pip3 install --user --break-system-packages PySMT || error_exit "Failed to install PySMT."
# pip3 install --user PySMT || error_exit "Failed to install PySMT."

# Step 8: Add Python 3.12 user directory to PATH
echo "Adding Python 3.12 user directory to PATH..."
echo 'export PATH="$HOME/Library/Python/3.12/bin:$PATH"' >> ~/.profile
export PATH="$HOME/Library/Python/3.12/bin:$PATH"
echo "Python 3.12 path added."

# Reload the profile again for the Python path change
source ~/.profile || error_exit "Failed to reload profile after adding Python path."

# Step 9: Clone and prepare ESBMC
# echo "Cloning CLARA_ESBMC repository..."
# git clone --progress https://github.com/proj-clarity-esbmc/clara_esbmc.git || error_exit "Failed to clone ESBMC repository."

# cd clara_esbmc || error_exit "Failed to navigate to CLARA_ESBMC directory."
# git checkout m-ali/clara/main || error_exit "Failed to switch to the m-ali/clara/main branch."
mkdir build && cd build || error_exit "Failed to create or navigate to build directory."

# Step 10: Verify CMake and Make installations
echo "Checking for CMake and Make installations..."
command -v cmake > /dev/null || error_exit "CMake not found. Please ensure it is installed."
command -v make > /dev/null || error_exit "Make not found. Please ensure it is installed."

# Step 11: Configure ESBMC build using CMake
if xcodebuild -version &> /dev/null; then
  echo "Xcode is installed. Using the Xcode SDK for building ESBMC."
  cmake .. -DENABLE_Z3=1 -DENABLE_SOLIDITY_FRONTEND=On -DENABLE_CLARITY_FRONTEND=On -DCMAKE_BUILD_TYPE=Debug -DC2GOTO_SYSROOT=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk || error_exit "CMake configuration failed."
else
  echo "Using Command Line Tools SDK for building ESBMC."
  cmake .. -DENABLE_Z3=1 -DENABLE_SOLIDITY_FRONTEND=On -DENABLE_CLARITY_FRONTEND=On -DCMAKE_BUILD_TYPE=Debug -DC2GOTO_SYSROOT=/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk || error_exit "CMake configuration failed."
fi

# Step 12: Build ESBMC
echo "Building CLARA_ESBMC..."
NUM_CORES=$(sysctl -n hw.ncpu)
echo "Using $NUM_CORES cores for the build."
make -j "$NUM_CORES" || error_exit "Failed to build CLARA_ESBMC."

# Step 13: Verify the ESBMC build
echo "Verifying CLARA_ESBMC installation..."
./src/esbmc/esbmc --version || error_exit "CLARA_ESBMC failed to run."
echo "CLARA_ESBMC build successful."

# Completion message
echo "CLARA_ESBMC setup completed successfully..."
