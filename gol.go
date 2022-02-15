package main

import (
	"crypto/rand"
	"flag"
	"fmt"
	"image"
	"image/color"
	"image/draw"
	"image/jpeg"
	"io"
	"os"
	"strconv"
)

/// TODO:
// 		- each generation has an array of unsigned int64 with enough bits to hold all rows*cols
// 		- each cell has a single bit in the array
// 		- each cell has eight neighboring bits in the array
// 		- neighbor array should be an [8]unsigned int64 whose values are the indices in the rows/cols array of the neighbors
// 		- each cell in each generation must check its neighbor bits to see if it's alive
// 		- each cell in each generation must set its bit if it's alive
// 		- checking for duplicates means looping through each index in the rows/cols array comparing it against previous generations by simple equality

type bits []uint64

type generation struct {
	bits    // an array of bits large to hold this generation's grid
	isalive bool
	count   int /* alive neighbors */
}

type Cell struct {
	neighbors   [8]*Cell
	thisGen     *generation
	prevGen     *generation
	bounds      image.Rectangle
	generations chan int /* receive generation to run from the pump */
}

type gridreq struct {
	i     int              /* generation index */
	imgch chan *image.RGBA /* accepts latest image, updates, then hands back */
}

type mycolor color.RGBA

var bc = mycolor{0xff, 0xff, 0xff, 0xff} /* default background color: white */
var gc = mycolor{0xf0, 0xf0, 0xf0, 0xff} /* default grid color: light grey */
var cc = mycolor{0x0, 0xff, 0x0, 0xff}   /* default cell color: green */
var imgx, imgy = 620, 620                /* default image height & width in pixels */
var rows, cols = 64, 64                  /* default number of rows & columns in grid */

var cells [][]Cell

func rc(r, c int) *Cell { /* constrain in board: wrap */
	if r < 0 {
		r = rows - 1
	} else if r >= rows {
		r = 0
	}
	if c < 0 {
		c = cols - 1
	} else if c >= cols {
		c = 0
	}
	return &cells[r][c]
}

func initialize(done chan int, gridch chan *gridreq) {
	cells = make([][]Cell, rows)
	for r := range cells {
		cells[r] = make([]Cell, cols)
	}
	b := make([]byte, rows*cols)
	io.ReadFull(rand.Reader, b)
	rh := (imgy - rows - 1) / rows
	cw := (imgx - cols - 1) / cols
	if rh < 3 || cw < 3 {
		fmt.Printf("error: too many rows or columns for image size "+
			"%dx%d: r %d c %d rh %d cw %d\n", imgx, imgy, rows, cols, rh, cw)
		os.Exit(1)
	}
	oldimgx, oldimgy := imgx, imgy
	imgx = cw*cols + cols + 1 /* grid lines are one pixel */
	imgy = rh*rows + rows + 1 /* grid lines are one pixel */
	xoff, yoff := (oldimgx-imgx)/2, (oldimgy-imgy)/2
	hline := image.NewRGBA(image.Rect(0, 0, imgx, 1))
	vline := image.NewRGBA(image.Rect(0, 0, 1, imgy))
	biota := image.NewRGBA(image.Rect(0, 0, cw, rh))
	grid := image.NewRGBA(image.Rect(0, 0, oldimgx, oldimgy))
	cellcolor := color.RGBA{cc.R, cc.G, cc.B, cc.A}
	gridcolor := color.RGBA{gc.R, gc.G, gc.B, gc.A}
	bckgndcolor := color.RGBA{bc.R, bc.G, bc.B, bc.A}
	draw.Draw(biota, biota.Bounds(), &image.Uniform{cellcolor}, image.Point{}, draw.Src)
	draw.Draw(hline, hline.Bounds(), &image.Uniform{gridcolor}, image.Point{}, draw.Src)
	draw.Draw(vline, vline.Bounds(), &image.Uniform{gridcolor}, image.Point{}, draw.Src)
	draw.Draw(grid, grid.Bounds(), &image.Uniform{bckgndcolor}, image.Point{}, draw.Src)
	x, y := xoff, yoff
	draw.Draw(grid, image.Rect(xoff, y, imgx+xoff, y+1), hline, image.Point{}, draw.Over)
	draw.Draw(grid, image.Rect(x, yoff, x+1, imgy+yoff), vline, image.Point{}, draw.Over)
	y += 1
	for r := 0; r < rows; r++ {
		x = xoff + 1
		for c := 0; c < cols; c++ {
			cell := &cells[r][c]
			cell.neighbors = [8]*Cell{
				rc(r-1, c-1), rc(r-1, c), rc(r-1, c+1),
				rc(r, c-1) /* self */, rc(r, c+1),
				rc(r+1, c-1), rc(r+1, c), rc(r+1, c+1),
			}
			cell.bounds = image.Rect(x+1, y+1, x+cw-1, y+rh-1)
			cell.generations = make(chan int)
			alive := b[r*cols+c]&0xa == 0xa
			size := uint64(rows*cols) / 64
			if size%64 != 0 {
				size++
			}
			cell.prevGen = nil
			cell.thisGen = &generation{bits: make([]uint64, size), isalive: alive, count: 0}
			go cell.live(biota, done, gridch, size)
			x += cw
			draw.Draw(grid, image.Rect(x, yoff, x+1, imgy+yoff), vline, image.Point{}, draw.Over)
			x += 1
		}
		y += rh
		draw.Draw(grid, image.Rect(xoff, y, imgx+xoff, y+1), hline, image.Point{}, draw.Over)
		y += 1
	}
	go gridgen(grid, gridch)
}

func (v *mycolor) Set(s string) error {
	i64, err := strconv.ParseInt(s, 0, 32)
	r := uint8(i64 >> 24 & 0xff)
	g := uint8(i64 >> 16 & 0xff)
	b := uint8(i64 >> 8 & 0xff)
	a := uint8(i64 & 0xff)
	*v = mycolor{r, g, b, a}
	return err
}

func (v *mycolor) String() string {
	return fmt.Sprintf("{ red: %#x, green: %#x, blue: %#x, alpha: %#x }",
		(*v).R, (*v).G, (*v).B, (*v).A)
}

func gridgen(grid draw.Image, gridch chan *gridreq) {
	b := grid.Bounds()
	imgarr := make([]*image.RGBA, 256)
	for r := range gridch {
		if r.i >= len(imgarr) {
			tmp := make([]*image.RGBA, 2*len(imgarr))
			imgarr = append(imgarr, tmp...)
		}
		if imgarr[r.i] == nil {
			gridcp := image.NewRGBA(b)
			draw.Draw(gridcp, b, grid, b.Min, draw.Src)
			imgarr[r.i] = gridcp
		}
		r.imgch <- imgarr[r.i]  /* pass this cell the latest grid */
		imgarr[r.i] = <-r.imgch /* wait for this cell update it & hand it back */
	}
}

func (cell *Cell) live(biota *image.RGBA, done chan int, gridch chan *gridreq, size uint64) {
	imgch := make(chan *image.RGBA)
	for i := range cell.generations {
		gridch <- &gridreq{i, imgch}
		dest := <-imgch /* get the latest image on which to draw */
		if cell.prevGen != nil {
			cell.thisGen = &generation{bits: make([]uint64, size), isalive: false, count: 0}
			if i > 0 {
				pg := cell.prevGen
				for k := range cell.neighbors {
					if cell.neighbors[k].prevGen.isalive {
						pg.count++
					}
				}
				if (pg.isalive && (pg.count == 2 || pg.count == 3)) || (pg.isalive == false && pg.count == 3) {
					cell.thisGen.isalive = true /* stays alive or is born */
				}
			}
		}
		if cell.thisGen.isalive {
			draw.Draw(dest, cell.bounds, biota, image.Point{}, draw.Over)
		}
		imgch <- dest /* return the image with this cell updated */
		done <- i
	}
	close(imgch)
	imgch = nil
}

func drawimg(i int, gridch chan *gridreq) error {
	file := fmt.Sprintf("./images/%d.jpg", i)
	f, err := os.Create(file)
	if err != nil {
		return err
	}
	defer f.Close()
	imgch := make(chan *image.RGBA)
	gridch <- &gridreq{i, imgch}
	pic := <-imgch /* get this generation's image */
	if err = jpeg.Encode(f, pic, nil); err != nil {
		return err
	}
	imgch <- pic /* must return the image */
	close(imgch)
	f.Sync()
	return nil
}

func main() {
	flag.IntVar(&rows, "r", rows, "number of rows")
	flag.IntVar(&cols, "c", cols, "number of columns")
	flag.IntVar(&imgx, "x", imgx, "image width")
	flag.IntVar(&imgy, "y", imgy, "image height")
	flag.Var(&bc, "b", "background color (default white)")
	flag.Var(&gc, "g", "grid color (default black)")
	flag.Var(&cc, "z", "cell color (default green)")
	flag.Parse()
	done := make(chan int) /* each cell responds when generation done */
	gridch := make(chan *gridreq)
	initialize(done, gridch)
	for i := 0; ; i++ { /* the pump */
		for r := 0; r < rows; r++ {
			for c := 0; c < cols; c++ {
				cells[r][c].generations <- i
			}
		}
		for j := 0; j < rows*cols; j++ {
			<-done
		}
		if err := drawimg(i, gridch); err != nil {
			fmt.Printf("error: %s\n", err.Error())
			break
		}
	}
	close(done)
	close(gridch)
	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			close(cells[r][c].generations)
		}
	}
}
