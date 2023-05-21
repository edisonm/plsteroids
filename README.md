# plsteroids
Scripts to facilitate testing and development of the Prolog libraries published here.

* Installation:
  
  Simply execute the next commands:

  ```
  git clone https://github.com/edisonm/plsteroids.git
  ```
  
* Script to execute common commands:

  ```
  make option
  make [target].option
  ```

  some few options are still in pltool.sh

  ```
  ./pltool.sh option [target]
  ```

  Where option is one of:
    patches tests testst testrtc teststrtc cover check checkt checkc loadall build doc

  If it is not one of them, it will execute it as a command in each submodule

  Since those commands are changing very often, documentation would be outdated,
  so it is better to read the source

  https://blog.codinghorror.com/learn-to-read-the-source-luke/
