Clarity frontend

### Build & Run

```sh
# configure
source ~/.profile
cmake .. -DENABLE_Z3=1 -DENABLE_SOLIDITY_FRONTEND=On -DENABLE_CLARITY_FRONTEND=On -DCMAKE_BUILD_TYPE=Debug -DC2GOTO_SYSROOT=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk

# build
make -j8

# run
./esbmc --clar counter.clar counter.clarast --clar_contract counter

# dump symbols table
./esbmc --clar counter.clar counter.clarast --clar_contract counter --symbol-table-only |& tee counter.syms

# generate c equivalent code
./esbmc --clar counter.clar counter.clarast --clar_contract counter --goto2c > counter.c

```
