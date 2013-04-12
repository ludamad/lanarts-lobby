mkdir -p build/
#function compile { for i in $@ ; do ghc -outputdir build/ -optl-static -static -O2 -threaded $i ; done }
function compile { for i in $@ ; do ghc -outputdir build/ -O2 -threaded $i ; done }
compile *.hs Contrib/*.hs
