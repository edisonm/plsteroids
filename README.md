# plsteroids
Scripts to facilitate testing and development of the Prolog libraries published here.

* Installation:
  
  Simply execute the next commands:

  git clone https://github.com/edisonm/plsteroids.git
  
  cd plsteroids

  git submodule init

  git submodule update

* Script to execute common commands:

  ./pltool.sh option [args]

  Where option is one of:
    patches tests testst testrtc teststrtc cover check checkt checkc loadall build

  If it is not one of them, it will execute it as a command in each submodule

* Useful git commands to keep in mind, since this project contain submodules:

  git pull --recurse-submodules
