echo "BEGIN BUILDING LIBRARY IN WINDOWS..."
del *.lib
call windowsbuild_clang.bat
echo "END BUILDING LIBRARY IN WINDOWS..."
