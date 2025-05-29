#!/usr/bin/env bash

#
# Script for building and installing a GNAT capable GCC cross-compiler from `x86-64-linux-elf` or
# `aarch64-linux-elf`, to `aarch64-elf` from source.
# This script has been adapted from the build instructions described at
# https://wiki.osdev.org/GNAT_Cross-Compiler
#

# The target triplet for the build.
export BUILD_TARGET="aarch64-elf"
# The install prefix.
export BUILD_PREFIX="${HOME}/opt/cross/${BUILD_TARGET}"

#export PATH=$HOME/.local/share/alire/toolchains/gnat_native_14.2.1_06bb3def/bin:$PATH

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/mpc/lib

# The host target triplet.
#export HOST="x86_64-pc-linux-gnu"
export HOST="aarch64-linux-gnu"

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

if [ $# != 1 ]; then
	echo "$0 <directory containing gcc-xx.x.x and bin-utils-x.xx directories>"
	exit 1
fi

source_dir=$1
if [[ ! -d "${source_dir}" ]]; then
   echo "*** ERROR: ${source_dir} not found"
   exit 1
fi

binutils_version=2.44
gcc_version=14.3.0
gdb_version=16.3

binutils_dir="binutils-${binutils_version}"
if [[ ! -d "${source_dir}/${binutils_dir}" ]]; then
   echo "*** ERROR: ${source_dir}/${binutils_dir} not found"
   exit 1
fi

gcc_dir="gcc-${gcc_version}"
if [[ ! -d "${source_dir}/${binutils_dir}" ]]; then
   echo "*** ERROR: ${source_dir}/${binutils_dir} not found"
   exit 1
fi

gdb_dir="gdb-${gdb_version}"
if [[ ! -d "${source_dir}/${binutils_dir}" ]]; then
   echo "*** ERROR: ${source_dir}/${binutils_dir} not found"
   exit 1
fi

build_dir=${source_dir}/build

if [[ ! -d "${build_dir}" ]]; then
	mkdir -p "${build_dir}" || exit 1
fi

echo "*****************************************************************"
echo "Step 0: Download prerequisites"
echo "*****************************************************************"

cd ${source_dir}/${gcc_dir}
./contrib/download_prerequisites
cd -

echo "*****************************************************************"
echo "Step 1: Build binutils"
echo "*****************************************************************"

cd "${build_dir}"

if [[ ! -d "${build_dir}/${binutils_dir}" ]]; then
	mkdir "${build_dir}/${binutils_dir}" || exit 1
fi

cd "${build_dir}/${binutils_dir}" || exit 1

${source_dir}/${binutils_dir}/configure      \
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

cd "${build_dir}"

if [[ ! -d "${build_dir}/${gcc_dir}" ]]; then
	mkdir "${build_dir}/${gcc_dir}" || exit 1
fi

cd "${build_dir}/${gcc_dir}" || exit 1

${source_dir}/${gcc_dir}/configure      \
	--target="${BUILD_TARGET}"          \
	--prefix="${BUILD_PREFIX}"          \
	--enable-languages="c"              \
	--disable-multilib                  \
	--disable-shared                    \
	--disable-nls                       \
	--without-headers || exit 1

make -j${concurrency} all-gcc || exit 1
make -j${concurrency} install-gcc || exit 1

echo "*****************************************************************"
echo "Step 3: Build gcc for C, C++ and Ada"
echo "*****************************************************************"

cd "${build_dir}"

if [[ ! -d "${build_dir}/${gcc_dir}" ]]; then
	mkdir "${build_dir}/${gcc_dir}" || exit 1
fi

cd "${build_dir}/${gcc_dir}" || exit 1

${source_dir}/${gcc_dir}/configure      \
	--target="${BUILD_TARGET}"          \
	--prefix="${BUILD_PREFIX}"          \
	--enable-languages="c,c++,ada"      \
	--disable-libada                    \
	--disable-nls                       \
	--disable-threads                   \
	--disable-multilib                  \
	--disable-shared                    \
	--without-headers || exit 1

make -j${concurrency} all-gcc || exit 1
make -j${concurrency} all-target-libgcc || exit 1
make -j${concurrency} -C gcc cross-gnattools ada.all.cross || exit 1
make -j${concurrency} install-strip-gcc install-target-libgcc || exit 1

echo "*****************************************************************"
echo "Step 4: Build gdb"
echo "*****************************************************************"

cd "${build_dir}"

if [[ ! -d "${build_dir}/${gdb_dir}" ]]; then
	mkdir "${build_dir}/${gdb_dir}" || exit 1
fi

cd "${build_dir}/${gdb_dir}" || exit 1

${source_dir}/${gdb_dir}/configure \
            --target="${BUILD_TARGET}" \
	    --prefix="${BUILD_PREFIX}" || exit 1

make -j${concurrency} || exit 1

make install || exit 1

echo "*****************************************************************"
echo "SUCCESSFUL BUILD"
echo "*****************************************************************"

ls -l ${HOME}/opt/cross/${BUILD_TARGET}*
ls -l ${HOME}/opt/cross/${BUILD_TARGET}*/bin
