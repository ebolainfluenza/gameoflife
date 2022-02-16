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
// 		- need to keep an array of genrations to check bits arrays against the current for given cycle depth so don't loop forever in equilibrium
// 		- debug it

type Generation struct {
	bits []uint64 /* an array of bits large to hold this generation's grid */
}

type Cell struct {
	index       uint   /* index into the generation bits array */
	mask        uint64 /* bitmask for this cell for the uint stored at the index in the generation bits array */
	neighbors   [8]*Cell
	bounds      image.Rectangle
	generations chan *Generation /* receive generation to run from the pump */
}

type GridReq struct {
	geni  int              /* generation index */
	imgch chan *image.RGBA /* accepts latest image, updates, then hands back */
}

type MyColor color.RGBA

var bc = MyColor{0xff, 0xff, 0xff, 0xff} /* default background color: white */
var gc = MyColor{0xf0, 0xf0, 0xf0, 0xff} /* default grid color: light grey */
var cc = MyColor{0x0, 0xff, 0x0, 0xff}   /* default cell color: green */
var imgx, imgy = 620, 620                /* default image height & width in pixels */
var rows, cols = 64, 64                  /* default number of rows & columns in grid */
var cycleDepth uint = 7                  /* default number of previous generations to check for duplicates of current */

var cells [][]Cell
var prevGens []*Generation /* a queue of previous generation pointers, len()=cycleDepth */

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

func initialize(done chan int, gridch chan *GridReq, size uint64) *Generation {
	cells = make([][]Cell, rows)
	for r := range cells {
		cells[r] = make([]Cell, cols)
	}
	b := make([]byte, rows*cols)
	io.ReadFull(rand.Reader, b)
	rh := (imgy - rows - 1) / rows
	cw := (imgx - cols - 1) / cols
	/* require 3 pixels for visibility's sake */
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
	thisGen := &Generation{bits: make([]uint64, size)}
	cellno := uint(0)    /* 0 to (rows*cols)-1 */
	bitsIndex := uint(0) /* index into the Generation.bits array */
	for r := 0; r < rows; r++ {
		x = xoff + 1
		for c := 0; c < cols; c++ {
			cell := &cells[r][c]
			if cellno >= 64 && cellno%64 == 0 {
				bitsIndex++
			}
			cell.index = bitsIndex          /* into the Generation.bits array */
			cell.mask = uint64(cellno % 64) /* of this cell's bit in the above index */
			cell.neighbors = [8]*Cell{
				rc(r-1, c-1), rc(r-1, c), rc(r-1, c+1),
				rc(r, c-1) /* self */, rc(r, c+1),
				rc(r+1, c-1), rc(r+1, c), rc(r+1, c+1),
			}
			cell.bounds = image.Rect(x+1, y+1, x+cw-1, y+rh-1)
			cell.generations = make(chan *Generation)
			if b[r*cols+c]&0xa == 0xa { /* set the cell's bit as alive for the initial generation */
				thisGen.bits[cell.index] |= cell.mask
			}
			go cell.live(biota, done, gridch)
			x += cw
			draw.Draw(grid, image.Rect(x, yoff, x+1, imgy+yoff), vline, image.Point{}, draw.Over)
			x += 1
			cellno++
		}
		y += rh
		draw.Draw(grid, image.Rect(xoff, y, imgx+xoff, y+1), hline, image.Point{}, draw.Over)
		y += 1
	}
	go gridgen(grid, gridch)
	return thisGen
}

func (v *MyColor) Set(s string) error {
	i64, err := strconv.ParseInt(s, 0, 32)
	r := uint8(i64 >> 24 & 0xff)
	g := uint8(i64 >> 16 & 0xff)
	b := uint8(i64 >> 8 & 0xff)
	a := uint8(i64 & 0xff)
	*v = MyColor{r, g, b, a}
	return err
}

func (v *MyColor) String() string {
	return fmt.Sprintf("{ red: %#x, green: %#x, blue: %#x, alpha: %#x }",
		(*v).R, (*v).G, (*v).B, (*v).A)
}

func gridgen(grid draw.Image, gridch chan *GridReq) {
	b := grid.Bounds()
	imgarr := make([]*image.RGBA, 256)
	for req := range gridch {
		if req.geni >= len(imgarr) {
			tmp := make([]*image.RGBA, 2*len(imgarr))
			imgarr = append(imgarr, tmp...)
		}
		if imgarr[req.geni] == nil {
			gridcp := image.NewRGBA(b)
			draw.Draw(gridcp, b, grid, image.Point{}, draw.Src)
			imgarr[req.geni] = gridcp
		}
		req.imgch <- imgarr[req.geni]  /* pass this cell/drawimg() the latest grid */
		imgarr[req.geni] = <-req.imgch /* wait for this cell/drawimg() hand it back (optionally updated) */
	}
}

func (cell *Cell) live(biota *image.RGBA, done chan int, gridch chan *GridReq) {
	var i int

	imgch := make(chan *image.RGBA)
	for thisGen := range cell.generations {
		var count uint   /* of this cell's previous generation's alive neighbors, zeroed each loop iteration */
		var isAlive bool /* is this cell alive in this generation, zeroed each loop iteration */

		gridch <- &GridReq{geni: i, imgch: imgch}
		dest := <-imgch /* get the latest image on which to draw */
		pg := prevGens[0]
		if pg != nil {
			wasAlive := (pg.bits[cell.index] & cell.mask) == cell.mask /* this cell's previous gen */
			for k := range cell.neighbors {
				mask := cell.neighbors[k].mask
				if pg.bits[cell.neighbors[k].index]&mask == mask {
					count++
				}
			}
			if (wasAlive && (count == 2 || count == 3)) || (wasAlive == false && count == 3) {
				isAlive = true                                /* stays alive or is born */
				thisGen.bits[cell.index] |= uint64(cell.mask) /* set this cell's bit as alive */
			}
		}
		if isAlive { /* 1st gen has no prevGen, but must still be drawn */
			draw.Draw(dest, cell.bounds, biota, image.Point{}, draw.Over)
		}
		imgch <- dest /* return the image with this cell updated */
		done <- i
		i++
	}
	close(imgch)
	imgch = nil
}

func drawimg(i int, gridch chan *GridReq) error {
	file := fmt.Sprintf("./images/%d.jpg", i)
	f, err := os.Create(file)
	if err != nil {
		return err
	}
	defer f.Close()
	imgch := make(chan *image.RGBA)
	gridch <- &GridReq{i, imgch}
	pic := <-imgch /* get this generation's image */
	if err = jpeg.Encode(f, pic, nil); err != nil {
		return err
	}
	imgch <- pic /* must return the image */
	close(imgch)
	f.Sync()
	return nil
}

// isDup checks the current generation's bits against each previous
// generations' bits for cycleDepth worth of generations.  It determines
// if equilibrium has been reached, and life is just cycling.  If thisGen
// is a dup of any one of the previous, then it is a dup.
func isDup(thisGen *Generation) bool {
	var r bool

	for i := range prevGens {
		if prevGens[i] == nil {
			continue
		}
		r = true
		for n := 0; n < len(thisGen.bits); n++ {
			if thisGen.bits[n] != prevGens[i].bits[n] {
				r = false
				break
			}
		}
	}
	return r
}

func main() {
	flag.IntVar(&rows, "r", rows, "number of rows")
	flag.IntVar(&cols, "c", cols, "number of columns")
	flag.IntVar(&imgx, "x", imgx, "image width")
	flag.IntVar(&imgy, "y", imgy, "image height")
	flag.UintVar(&cycleDepth, "d", cycleDepth, "duplicate cycle depth")
	flag.Var(&bc, "b", "background color (default white)")
	flag.Var(&gc, "g", "grid color (default black)")
	flag.Var(&cc, "z", "cell color (default green)")
	flag.Parse()
	done := make(chan int) /* each cell responds when generation done */
	gridch := make(chan *GridReq)
	prevGens = make([]*Generation, cycleDepth)
	size := uint64(rows*cols) / 64
	if size%64 != 0 {
		size++
	}
	thisGen := initialize(done, gridch, size)
	/*
	 * unfortunately we can't just let the cells go wild. in order to
	 * keep memory usage low, we have to keep them in lock-step with
	 * one another generationally, to keep track of this gen & prev gen.
	 */
	for i := 0; i >= 0; i++ { /* the pump */
		for r := 0; r < rows; r++ {
			for c := 0; c < cols; c++ {
				cells[r][c].generations <- thisGen
			}
		}
		/* wait for all cells to finish drawing themselves on the grid */
		for j := 0; j < rows*cols; j++ {
			<-done
		}
		if err := drawimg(i, gridch); err != nil {
			fmt.Printf("drawimg: %s\n", err.Error())
			break
		}
		/* stop if there's a duplicate image found in the cycle depth */
		if isDup(thisGen) {
			break
		}
		/* put this gen at the front, and push the oldest off the back */
		prevGens = append([]*Generation{thisGen}, prevGens[:cycleDepth-1]...)
		thisGen = &Generation{bits: make([]uint64, size)}
	}
	close(done)
	close(gridch)
	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			close(cells[r][c].generations)
		}
	}
}
