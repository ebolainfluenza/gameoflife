package main

import (
	"crypto/md5"
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

type Generation struct {
	isAlive bool
	count   int /* alive neighbors */
}

type Cell struct {
	neighbors [8]*Cell
	gens      []*Generation /* successive generations of this cell */
	bounds    image.Rectangle
	generate  chan int64 /* receive generation to run from the pump */
}

type GridReq struct {
	i     int64            /* generation index */
	imgch chan *image.RGBA /* accepts latest image, updates, then hands back */
}

type MyColor color.RGBA

var bc = MyColor{0xff, 0xff, 0xff, 0xff} /* default background color: white */
var gc = MyColor{0xf0, 0xf0, 0xf0, 0xff} /* default grid color: light grey */
var cc = MyColor{0x0, 0xff, 0x0, 0xff}   /* default cell color: green */

var imgx, imgy = 620, 620 /* default image height & width constraints */

var rows, cols = 64, 64
var cells [][]Cell

const cbuflen = 6        /* max period for repeat patterns */
var cbuf [cbuflen][]byte /* circular buffer of md5 sums */
var cbufi int            /* where current generation's md5 sum is stored */

var sss int64 = 1024 /* slice start size */

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

func initialize(done chan int64, gridch chan *GridReq) {
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
			cell.generate = make(chan int64)
			alive := b[r*cols+c]&0xa == 0xa
			cell.gens = make([]*Generation, sss)
			cell.gens[0] = &Generation{isAlive: alive, count: 0}
			go cell.live(alive, biota, done, gridch)
			x += cw
			draw.Draw(grid, image.Rect(x, yoff, x+1, imgy+yoff), vline, image.Point{}, draw.Over)
			x += 1
		}
		y += rh
		draw.Draw(grid, image.Rect(xoff, y, imgx+xoff, y+1), hline, image.Point{}, draw.Over)
		y += 1
	}
	go gridGen(grid, gridch)
}

func (c *MyColor) Set(s string) error {
	i64, err := strconv.ParseInt(s, 0, 32)
	r := uint8(i64 >> 24 & 0xff)
	g := uint8(i64 >> 16 & 0xff)
	b := uint8(i64 >> 8 & 0xff)
	a := uint8(i64 & 0xff)
	*c = MyColor{r, g, b, a}
	return err
}

func (c *MyColor) String() string {
	return fmt.Sprintf("{ red: %#x, green: %#x, blue: %#x, alpha: %#x }",
		c.R, c.G, c.B, c.A)
}

// gridGen receives requests for the current grid image from cells (and drawImg()),
// and passes the current image to the next cell that is ready to draw onto it.
func gridGen(grid draw.Image, gridch chan *GridReq) {
	b := grid.Bounds()
	imgarr := make([]*image.RGBA, sss)
	for r := range gridch {
		if r.i >= int64(len(imgarr)) {
			tmp := make([]*image.RGBA, 2*len(imgarr))
			imgarr = append(imgarr, tmp...)
		}
		// the 1st cell to draw this new generation will have an empty/nil image
		if imgarr[r.i] == nil {
			emptyGrid := image.NewRGBA(b)
			draw.Draw(emptyGrid, b, grid, image.Point{}, draw.Src)
			imgarr[r.i] = emptyGrid
		}
		r.imgch <- imgarr[r.i]  /* pass this cell the latest grid */
		imgarr[r.i] = <-r.imgch /* wait for this cell update it & hand it back */
	}
}

// Cell.live is the lifetime of a cell.  It loops through each generation,
// and if it's alive in this generation, it draws itself on the current image.
func (cell *Cell) live(alive bool, biota *image.RGBA, done chan int64, gridch chan *GridReq) {
	imgch := make(chan *image.RGBA)
	for i := range cell.generate {
		gridch <- &GridReq{i, imgch}
		dest := <-imgch /* get the latest image on which to draw */
		if i >= int64(len(cell.gens)) {
			tmp := make([]*Generation, 2*len(cell.gens))
			cell.gens = append(cell.gens, tmp...)
		}
		if i > 0 {
			cell.gens[i] = &Generation{isAlive: false, count: 0}
			pg := cell.gens[i-1] /* previous generation */
			for k := range cell.neighbors {
				if cell.neighbors[k].gens[i-1].isAlive {
					pg.count++
				}
			}
			if (pg.isAlive && (pg.count == 2 || pg.count == 3)) ||
				(pg.isAlive == false && pg.count == 3) {
				cell.gens[i].isAlive = true /* stays alive or is born */
			}
		}
		if cell.gens[i].isAlive {
			draw.Draw(dest, cell.bounds, biota, image.Point{}, draw.Over)
		}
		imgch <- dest /* return the image with this cell updated */
		done <- i
	}
	close(imgch)
	imgch = nil
}

// drawImg grabs the current generation's final image, encodes the bits as a JPEG, and saves it
// to a file.  It returns true if it's a duplicate of another image in the ring, false otherwise.
func drawImg(i int64, gridch chan *GridReq) bool {
	file := fmt.Sprintf("./images/life%d.jpg", i)
	f, err := os.Create(file)
	if err != nil {
		panic(err)
	}
	defer f.Close()
	// here, we pretend we're a cell, grabbing the image,
	// updating it (saving it to a file), then returning it
	imgch := make(chan *image.RGBA)
	gridch <- &GridReq{i, imgch}
	pic := <-imgch /* get this generation's completed image */
	err = jpeg.Encode(f, pic, nil)
	if err != nil {
		panic(err)
	}
	imgch <- pic /* must return the image */
	close(imgch)
	f.Sync()
	return isdup(f)
}

/* compares md5 sums of recent (pattern period of cbuflen) images for duplicates */
func isdup(f *os.File) bool {
	flen, err := f.Seek(0, 2)
	if err != nil {
		panic(err)
	}
	_, err = f.Seek(0, 0)
	if err != nil {
		panic(err)
	}
	b := make([]byte, flen)
	n, err2 := f.Read(b)
	if err2 != nil {
		panic(err2)
	}
	if int64(n) != flen {
		panic("bytes read != file length")
	}
	h := md5.New()
	n, err = h.Write(b)
	if err != nil {
		panic(err)
	}
	if n != len(b) {
		panic("n != len(b)")
	}
	sum := h.Sum(nil)
	i := cbufi
	for {
		if cbuf[i] != nil {
			dup := true
			for j := 0; j < md5.Size; j++ {
				if cbuf[i][j] != sum[j] {
					dup = false
					break
				}
			}
			if dup {
				return true
			}
		}
		i--
		if i < 0 {
			i = cbuflen - 1
		}
		if i == cbufi { /* full circle */
			break
		}
	}
	cbuf[cbufi] = sum
	cbufi++
	if cbufi == cbuflen {
		cbufi = 0
	}
	return false
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
	done := make(chan int64)
	gridch := make(chan *GridReq)
	initialize(done, gridch)
	for i := int64(0); i >= 0; i++ { /* the generation pump */
		for r := 0; r < rows; r++ {
			for c := 0; c < cols; c++ {
				cells[r][c].generate <- i
			}
		}
		for j := 0; j < rows*cols; j++ {
			<-done // wait for all cells to make their mark
		}
		if drawImg(i, gridch) {
			break
		}
	}
	close(done)
	close(gridch)
	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			close(cells[r][c].generate)
		}
	}
}
