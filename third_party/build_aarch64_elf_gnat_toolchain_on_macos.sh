#!/usr/bin/env bash

# Recipe for building and installing a GNAT capable GCC cross-compiler from `x86-64-linux-elf`
# to `i686-elf` from source.

# The target triplet for the build.
export BUILD_TARGET="aarch64-elf"
# The install prefix.
export BUILD_PREFIX="${HOME}/opt/cross/${BUILD_TARGET}"

export PATH=/Applications/ArmGNUToolchain/14.2.rel1/aarch64-none-elf/bin:/opt/gcc-14.2.0-3-aarch64/bin:$PATH

# The host target triplet.
#??? export HOST="aarch64-apple-darwin23-gcc"

# Update the PATH variable for this script so it includes the build directory.
# This will add our newly built cross-compiler to the PATH.
# After the initial GCC cross-compiler build this initial cross-compiler
# will be used to build a version with Ada support.
#??? export PATH="${BUILD_PREFIX}/bin:${PATH}"

# The concurrency to use during the build process.
# Adjust this based upon the capabilities of the host system.
concurrency=8

# The directory where local library dependencies are installed.
# This is used to find installed dependencies such as MPFR, GMP, MPC, etc.
# This is the default location for installations using aptitude.
# Adjust this as necessary for the target system.
local_lib_dir="/usr/local"

gcc_version=14.2.0
gcc_dir="gcc-${gcc_version}"
source_dir=$PWD
build_dir=${source_dir}/build

binutils_version=2.44
binutils_dir="binutils-${binutils_version}"

echo "*****************************************************************"
echo "Step 1: Build binutils"
echo "*****************************************************************"

cd "${build_dir}" || exit 1

if [[ ! -d "${build_dir}/${binutils_dir}" ]]; then
	mkdir "${build_dir}/${binutils_dir}" || exit 1
fi

cd "${build_dir}/${binutils_dir}" || exit 1

${source_dir}/${binutils_dir}/configure          \
	--target="${BUILD_TARGET}"               \
	--prefix="${BUILD_PREFIX}"               \
	--host="${HOST}"                         \
	--disable-nls                            \
	--disable-multilib                       \
	--disable-shared                         \
        --with-sysroot || exit 1

# Check the host environment.
make configure-host || exit 1
make -j${concurrency} || exit 1
make -j${concurrency} install || exit 1

echo "*****************************************************************"
echo "Step 2: Build gcc for C only"
echo "*****************************************************************"

cd "${build_dir}" || exit 1

if [[ ! -d "${build_dir}/${gcc_dir}" ]]; then
	mkdir "${build_dir}/${gcc_dir}" || exit 1
fi

cd "${build_dir}/${gcc_dir}" || exit 1

${source_dir}/configure          \
	--target="${BUILD_TARGET}"          \
	--prefix="${BUILD_PREFIX}"          \
	--enable-languages="c"              \
	--disable-multilib                  \
	--disable-shared                    \
	--disable-nls                       \
	--with-gmp=/opt/homebrew/Cellar/gmp/6.3.0 \
	--with-mpc=/opt/homebrew/Cellar/libmpc/1.3.1 \
	--with-mpfr=/opt/homebrew/Cellar/mpfr/4.2.1 \
	--without-headers || exit 1

make -j${concurrency} all-gcc || exit 1
make -j${concurrency} install-gcc || exit 1

echo "*****************************************************************"
echo "Step 3: Build gcc for C, C++ and Ada"
echo "*****************************************************************"

cd "${build_dir}" || exit 1

if [[ ! -d "${build_dir}/${gcc_dir}" ]]; then
	mkdir "${build_dir}/${gcc_dir}" || exit 1
fi

cd "${build_dir}/${gcc_dir}" || exit 1

${source_dir}/configure          \
	--target="${BUILD_TARGET}"          \
	--prefix="${BUILD_PREFIX}"          \
	--enable-languages="c,c++,ada"      \
	--disable-libada                    \
	--disable-nls                       \
	--disable-threads                   \
	--disable-multilib                  \
	--disable-shared                    \
	--with-gmp=/opt/homebrew/Cellar/gmp/6.3.0 \
	--with-mpc=/opt/homebrew/Cellar/libmpc/1.3.1 \
	--with-mpfr=/opt/homebrew/Cellar/mpfr/4.2.1 \
	--without-headers || exit 1

make -j${concurrency} all-gcc || exit 1
make -j${concurrency} all-target-libgcc || exit 1
make -j${concurrency} -C gcc cross-gnattools ada.all.cross || exit 1
make -j${concurrency} install-strip-gcc install-target-libgcc || exit 1

ls -l ${HOME}/opt/cross/${BUILD_TARGET}*
