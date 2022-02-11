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

type generation struct {
	isalive bool
	count   int /* alive neighbors */
}

type cell struct {
	neighbors [8]*cell
	gens      []*generation /* successive generations of this cell */
	bounds    image.Rectangle
	generate  chan int /* receive generation to run from the pump */
}

type gridreq struct {
	i     int              /* generation index */
	imgch chan *image.RGBA /* accepts latest image, updates, then hands back */
}

type mycolor color.RGBA

var bc = mycolor{0xff, 0xff, 0xff, 0xff} /* default background color: white */
var gc = mycolor{0xf0, 0xf0, 0xf0, 0xff} /* default grid color: light grey */
var cc = mycolor{0x0, 0xff, 0x0, 0xff}   /* default cell color: green */
var imgx, imgy = 620, 620                /* image height & width constraints */
var sss = 1024                           /* slice start size */

var rows, cols = 64, 64
var cells [][]cell

const cbuflen = 6        /* max period for repeat patterns */
var cbuf [cbuflen][]byte /* circular buffer of md5 sums */
var cbufi int            /* where current generation's md5 sum is stored */

func rc(r, c int) *cell { /* constrain in board: wrap */
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
	cells = make([][]cell, rows)
	for r := range cells {
		cells[r] = make([]cell, cols)
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
	draw.Draw(biota, biota.Bounds(), &image.Uniform{cellcolor}, image.ZP, draw.Src)
	draw.Draw(hline, hline.Bounds(), &image.Uniform{gridcolor}, image.ZP, draw.Src)
	draw.Draw(vline, vline.Bounds(), &image.Uniform{gridcolor}, image.ZP, draw.Src)
	draw.Draw(grid, grid.Bounds(), &image.Uniform{bckgndcolor}, image.ZP, draw.Src)
	x, y := xoff, yoff
	draw.Draw(grid, image.Rect(xoff, y, imgx+xoff, y+1), hline, image.ZP, draw.Over)
	draw.Draw(grid, image.Rect(x, yoff, x+1, imgy+yoff), vline, image.ZP, draw.Over)
	y += 1
	for r := 0; r < rows; r++ {
		x = xoff + 1
		for c := 0; c < cols; c++ {
			t := &cells[r][c]
			t.neighbors = [8]*cell{
				rc(r-1, c-1), rc(r-1, c), rc(r-1, c+1),
				rc(r, c-1) /* self */, rc(r, c+1),
				rc(r+1, c-1), rc(r+1, c), rc(r+1, c+1),
			}
			t.bounds = image.Rect(x+1, y+1, x+cw-1, y+rh-1)
			t.generate = make(chan int)
			alive := b[r*cols+c]&0xa == 0xa
			t.gens = make([]*generation, sss)
			t.gens[0] = &generation{isalive: alive, count: 0}
			go live(t, alive, biota, done, gridch)
			x += cw
			draw.Draw(grid, image.Rect(x, yoff, x+1, imgy+yoff), vline, image.ZP, draw.Over)
			x += 1
		}
		y += rh
		draw.Draw(grid, image.Rect(xoff, y, imgx+xoff, y+1), hline, image.ZP, draw.Over)
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
	imgarr := make([]*image.RGBA, sss)
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

func live(t *cell, alive bool, biota *image.RGBA, done chan int, gridch chan *gridreq) {
	imgch := make(chan *image.RGBA)
	for i := range t.generate {
		gridch <- &gridreq{i, imgch}
		dest := <-imgch /* get the latest image on which to draw */
		if i >= len(t.gens) {
			tmp := make([]*generation, 2*len(t.gens))
			t.gens = append(t.gens, tmp...)
		}
		if t.gens[i] == nil {
			t.gens[i] = &generation{false, 0}
			if i > 0 {
				pg := t.gens[i-1] /* previous generation */
				for k := range t.neighbors {
					if t.neighbors[k].gens[i-1].isalive {
						pg.count++
					}
				}
				if (pg.isalive && (pg.count == 2 || pg.count == 3)) ||
					(pg.isalive == false && pg.count == 3) {
					t.gens[i].isalive = true /* stays alive or is born */
					draw.Draw(dest, t.bounds, biota, image.ZP, draw.Over)
				}
			}
		}
		imgch <- dest /* return the image with this cell updated */
		done <- i
	}
	close(imgch)
	imgch = nil
}

func drawimg(i int, gridch chan *gridreq) bool {
	file := fmt.Sprintf("./images/life%d.jpg", i)
	f, err := os.Create(file)
	if err != nil {
		panic(err)
	}
	defer f.Close()
	imgch := make(chan *image.RGBA)
	gridch <- &gridreq{i, imgch}
	pic := <-imgch /* get this generation's image */
	err = jpeg.Encode(f, pic, nil)
	if err != nil {
		panic(err)
	}
	imgch <- pic /* must return the image */
	close(imgch)
	f.Sync()
	return (isdup(f) == false)
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
		i -= 1
		if i < 0 {
			i = cbuflen - 1
		}
		if i == cbufi { /* full circle */
			break
		}
	}
	cbufi += 1
	if cbufi == cbuflen {
		cbufi = 0
	}
	cbuf[cbufi] = sum
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
	done := make(chan int) /* each cell responds when generation done */
	gridch := make(chan *gridreq)
	initialize(done, gridch)
	for i := 0; ; i++ { /* the pump */
		for r := 0; r < rows; r++ {
			for c := 0; c < cols; c++ {
				cells[r][c].generate <- i
			}
		}
		for j := 0; j < rows*cols; j++ {
			<-done
		}
		if drawimg(i, gridch) == false {
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
