#!/usr/bin/env sh

## from: https://stackoverflow.com/a/39730469/453735

mkdir -p ~/.R

if [ "${COMPILER}" = "gcc" ]; then
	echo "CC=gcc-${VER} -std=c99"      >> ~/.R/Makevars
	echo "CXX=g++-${VER}"              >> ~/.R/Makevars
	echo "CXX1X=g++-${VER} -std=c++0x" >> ~/.R/Makevars
	echo "CXX11=g++-${VER} -std=c++11" >> ~/.R/Makevars
	echo "FC=gfortran-${VER}"          >> ~/.R/Makevars
    echo "SHLIB_CXXLD=g++-${VER}"      >> ~/.R/Makevars
	echo "F77=gfortran-${VER}"         >> ~/.R/Makevars
	echo "MAKE=make -j4"               >> ~/.R/Makevars
fi

if [ "${COMPILER}" = "clang" ]; then
    # https://github.com/travis-ci/travis-ci/issues/8613
    export LD_LIBRARY_PATH=/usr/local/clang/lib:$LD_LIBRARY_PATH
    echo "CC=clang -std=gnu99"      >> ~/.R/Makevars
    echo "CXX=clang++"              >> ~/.R/Makevars
    echo "CXX11=clang++ -std=c++11" >> ~/.R/Makevars
fi
