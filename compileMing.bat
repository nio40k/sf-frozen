SET PATH=C:\Programs\MinGW\mingw64\bin;C:\Programs\MinGW\msys\1.0\bin;%PATH%

mingw32-make profile-build COMP=mingw ARCH=x86-64-modern -j 3
del *.o
del syzygy\*.o

pause