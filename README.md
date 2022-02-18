# gameoflife
Conway's Game of Life

```
1- clone this repo
2- cd into <cloned_directory>
3- chmod +x gol.sh
4- run ./gol.sh in a unix/linux environment, or build & run the binary on another environment, passing it params if desired (see gol.sh)
5- browse to <cloned_directory>/index.html on your preferred browser (Firefox has had some issues running the gol.js lately, but Opera works well)
6- use the buttons on the web page to start/stop/continue/step-forward/step-backward through the images representing the generations of life
7- enjoy!
```

# Usage:

```
$ ./gol --help
Usage of ./gol:
  -b value
    	background color (default white) (default { red: 0xff, green: 0xff, blue: 0xff, alpha: 0xff })
  -c int
    	number of columns (default 64)
  -d uint
    	duplicate cycle depth (default 7)
  -g value
    	grid color (default black) (default { red: 0xf0, green: 0xf0, blue: 0xf0, alpha: 0xff })
  -r int
    	number of rows (default 64)
  -x int
    	image width (default 620)
  -y int
    	image height (default 620)
  -z value
    	cell color (default green) (default { red: 0x0, green: 0xff, blue: 0x0, alpha: 0xff })
$ 
```
