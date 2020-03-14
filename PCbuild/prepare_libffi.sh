#!/usr/bin/bash

set -ex
export PATH="/usr/bin:$PATH"

cd "$(dirname "$(realpath "$0")")"/../externals/libffi-3.3.0-rc0-r1
export LIBFFI_SOURCE="$(pwd)"

export MSVCC="$(realpath msvcc.sh)" && echo MSVCC: ${MSVCC}
export VCVARS_PLATFORM=x64
export BUILD=x86_64-w64-cygwin
export HOST=x86_64-w64-cygwin

export ARCH=amd64
export ARTIFACTS="$LIBFFI_SOURCE/x86_64-w64-cygwin"
export ASSEMBLER=-m64
export SRC_ARCHITECTURE=x86

export LIBFFI_OUT="$(dirname "$(realpath "$0")")"/../libffi
export _LIBFFI_OUT="$LIBFFI_OUT/$ARCH"

echo Cleaning "$_LIBFFI_OUT"
rm -rv "$_LIBFFI_OUT" || :
mkdir -p "$_LIBFFI_OUT"

echo Check Makefile.in
if [ ! -f "Makefile.in" ]; then
    ./autogen.sh
fi

echo Configuring
./configure CC="$MSVCC -DFFI_BUILDING $ASSEMBLER $BUILD_PDB" CXX="$MSVCC -DFFI_BUILDING $ASSEMBLER $BUILD_PDB" LD='link -nologo' CPP='cl -nologo -EP' CXXCPP='cl -nologo -EP' CPPFLAGS='' $BUILD_NOOPT NM='dumpbin -symbols' STRIP=':' --build=$BUILD --host=$HOST --enable-static

echo Building
# rm -v include/ffi.h || :
cp "src/$SRC_ARCHITECTURE/ffitarget.h" include/
make -j

echo Copying
mkdir -p "$_LIBFFI_OUT"/include
cp "$ARTIFACTS"/.libs/libffi* "$_LIBFFI_OUT"
cp "$ARTIFACTS"/fficonfig.h "$_LIBFFI_OUT"/include
cp "$ARTIFACTS"/include/*.h "$_LIBFFI_OUT"/include
