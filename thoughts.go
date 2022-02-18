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

type Generation struct {
	geni  int         /* generation index */
	bits  []uint64    /* an array of bits large to hold this generation's grid */
	image *image.RGBA /* the drawn grid and cells */
}

type Cell struct {
	index       uint   /* index into the generation bits array */
	mask        uint64 /* bitmask for this cell for the uint stored at the index in the generation bits array */
	neighbors   [8]*Cell
	bounds      image.Rectangle
	generations chan *Generation /* receive generation to run from the pump */
}

// type GridReq struct {
// 	geni  int              /* generation index */
// 	imgch chan *image.RGBA /* accepts latest image, updates, then hands back */
// }

type MyColor color.RGBA

var bc = MyColor{0xff, 0xff, 0xff, 0xff} /* default background color: white */
var gc = MyColor{0xf0, 0xf0, 0xf0, 0xff} /* default grid color: light grey */
var cc = MyColor{0x0, 0xff, 0x0, 0xff}   /* default cell color: green */
var imgx, imgy = 620, 620                /* default image height & width in pixels */
var rows, cols = 64, 64                  /* default number of rows & columns in grid */
var cycleDepth uint = 7                  /* default number of previous generations to check for duplicates of current */
var ZP = image.Point{}                   /* the image Zero Point, both X & Y are 0 (constant) */

var cells [][]Cell
var prevGens []*Generation

func rc(cells [][]Cell, r, c int) *Cell { /* constrain in board: wrap */
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

// emtpyGrid creates a new image with an empty grid on which to draw.
func emptyGrid(grid *image.RGBA) *image.RGBA {
	b := grid.Bounds()
	img := image.NewRGBA(b)
	draw.Draw(img, b, grid, ZP, draw.Src)
	return img
}

func initialize(size uint64) (*image.RGBA, *Generation) { //done chan int, gridch chan *GridReq, size uint64) *Generation { //, prevGens []*Generation, cells [][]Cell) *Generation {
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
	draw.Draw(biota, biota.Bounds(), &image.Uniform{cellcolor}, ZP, draw.Src)
	draw.Draw(hline, hline.Bounds(), &image.Uniform{gridcolor}, ZP, draw.Src)
	draw.Draw(vline, vline.Bounds(), &image.Uniform{gridcolor}, ZP, draw.Src)
	draw.Draw(grid, grid.Bounds(), &image.Uniform{bckgndcolor}, ZP, draw.Src)
	x, y := xoff, yoff
	draw.Draw(grid, image.Rect(xoff, y, imgx+xoff, y+1), hline, ZP, draw.Over)
	draw.Draw(grid, image.Rect(x, yoff, x+1, imgy+yoff), vline, ZP, draw.Over)
	y += 1
	rndm := make([]byte, rows*cols)
	io.ReadFull(rand.Reader, rndm)
	thisGen := &Generation{geni: 0, bits: make([]uint64, size), image: emptyGrid(grid)}
	cellno := uint(0)    /* 0 to (rows*cols)-1 */
	bitsIndex := uint(0) /* index into the Generation.bits array */
	for r := 0; r < rows; r++ {
		x = xoff + 1
		for c := 0; c < cols; c++ {
			cell := &cells[r][c]
			if cellno >= 64 && cellno%64 == 0 {
				bitsIndex++
			}
			cell.index = bitsIndex             /* into the Generation.bits array */
			cell.mask = 1 << uint64(cellno%64) /* of this cell's bit in the above index */
			cell.neighbors = [8]*Cell{
				rc(cells, r-1, c-1), rc(cells, r-1, c), rc(cells, r-1, c+1),
				rc(cells, r, c-1) /* self */, rc(cells, r, c+1),
				rc(cells, r+1, c-1), rc(cells, r+1, c), rc(cells, r+1, c+1),
			}
			cell.bounds = image.Rect(x+1, y+1, x+cw-1, y+rh-1)
			cell.generations = make(chan *Generation)
			/* determine if this cell's bit is alive for the initial generation */
			isAlive := false
			if rndm[r*cols+c]&0xa == 0xa { /* arbitrary */
				thisGen.bits[cell.index] |= cell.mask
				isAlive = true
			}
			go cell.live(biota, isAlive) //done, gridch, isAlive) //, prevGens)
			x += cw
			draw.Draw(grid, image.Rect(x, yoff, x+1, imgy+yoff), vline, ZP, draw.Over)
			x += 1
			cellno++
		}
		y += rh
		draw.Draw(grid, image.Rect(xoff, y, imgx+xoff, y+1), hline, ZP, draw.Over)
		y += 1
	}
	// go gridGen(grid, gridch)
	return grid, thisGen
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

// // gridGen synchronizes the passing around of this generation's image.
// func gridGen(grid draw.Image, gridch chan *GridReq) {
// 	var img *image.RGBA

// 	geni := -1
// 	b := grid.Bounds()
// 	for req := range gridch {
// 		if req.geni != geni { /* create a new generation's empty grid image */
// 			img = image.NewRGBA(b)
// 			draw.Draw(img, b, grid, ZP, draw.Src)
// 			geni = req.geni
// 		}
// 		req.imgch <- img  /* pass this cell/drawimg() the latest grid */
// 		img = <-req.imgch /* wait for this cell/drawimg() hand it back (optionally updated) */
// 	}
// }

func (cell *Cell) live(biota *image.RGBA, isAlive bool) { //done chan int, gridch chan *GridReq, isAlive bool) { //, prevGens []*Generation) {
	// imgch := make(chan *image.RGBA)
	for thisGen := range cell.generations {
		// gridch <- &GridReq{geni: thisGen.geni, imgch: imgch}
		// dest := <-imgch /* get the latest image on which to draw */
		pg := prevGens[0]
		if pg != nil {
			count := 0 /* of this cell's previous generation's alive neighbors */
			for i := range cell.neighbors {
				mask := cell.neighbors[i].mask
				if pg.bits[cell.neighbors[i].index]&mask == mask {
					count++
				}
			}
			isAlive = false
			wasAlive := (pg.bits[cell.index] & cell.mask) == cell.mask /* this cell's previous gen */
			if (wasAlive && (count == 2 || count == 3)) || (wasAlive == false && count == 3) {
				isAlive = true                                /* stays alive or is born */
				thisGen.bits[cell.index] |= uint64(cell.mask) /* set this cell's bit as alive */
			}
		}
		if isAlive { /* 1st gen has no prevGen, but must still be drawn */
			draw.Draw(thisGen.image, cell.bounds, biota, ZP, draw.Over)
		}
		// imgch <- dest /* return the image with this cell updated */
		cell.generations <- thisGen /* return the image with this cell updated */
		// done <- thisGen.geni
	}
	// close(imgch)
	// imgch = nil
}

func drawimg(geni int, thisGen *Generation) error { //gridch chan *GridReq) error {
	f, err := os.Create(fmt.Sprintf("./images/%d.jpg", geni))
	if err != nil {
		return err
	}
	defer f.Close()
	// imgch := make(chan *image.RGBA)
	// gridch <- &GridReq{geni: geni, imgch: imgch}
	// pic := <-imgch /* get this generation's image */
	// if err = jpeg.Encode(f, pic, nil); err != nil {
	if err = jpeg.Encode(f, thisGen.image, nil); err != nil {
		return err
	}
	// imgch <- pic /* must return the image */
	// close(imgch)
	// imgch = nil
	f.Sync()
	return nil
}

// isDup checks the current generation's bits against each previous
// generations' bits for cycleDepth worth of generations.  It determines
// if equilibrium has been reached, and life is just cycling.  If thisGen
// is a dup of any one of the previous generations, then it is a dup.
func isDup(thisGen *Generation) bool { //, prevGens []*Generation) bool {
	for i := 0; i < len(prevGens); i++ {
		if prevGens[i] == nil {
			continue
		}
		allBitsSame := true
		for n := 0; n < len(thisGen.bits); n++ {
			if thisGen.bits[n] != prevGens[i].bits[n] {
				allBitsSame = false
				break /* must check all prevGens */
			}
		}
		if allBitsSame {
			return true
		}
	}
	return false
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
	cells = make([][]Cell, rows)
	for r := range cells {
		cells[r] = make([]Cell, cols)
	}
	// done := make(chan int) /* each cell responds when generation done */
	// gridch := make(chan *GridReq)
	prevGens = make([]*Generation, cycleDepth)
	size := uint64(rows*cols) / 64 /* one bit per cell */
	if size == 0 || size%64 != 0 {
		size++
	}
	grid, thisGen := initialize(size) //done, size) //gridch, size) //, prevGens, cells)
	/*
	 * unfortunately we can't just let the cells go wild. in order to
	 * keep memory usage low, we have to keep them in lock-step with
	 * one another generationally, to keep track of this gen & prev gen.
	 */
	for i := 0; i >= 0; i++ { /* the pump */
		for r := 0; r < rows; r++ {
			for c := 0; c < cols; c++ {
				cells[r][c].generations <- thisGen
				thisGen = <-cells[r][c].generations
			}
		}
		// /* wait for all cells to finish drawing themselves on the grid */
		// for j := 0; j < rows*cols; j++ {
		// 	<-done
		// }
		// if err := drawimg(i, gridch); err != nil {
		if err := drawimg(i, thisGen); err != nil {
			fmt.Printf("drawimg: %s\n", err.Error())
			break
		}
		/* stop if there's a duplicate image found in the cycle depth */
		if isDup(thisGen) { //, prevGens) {
			break
		}
		prevGens = append([]*Generation{thisGen}, prevGens[:cycleDepth-1]...)
		thisGen = &Generation{geni: i + 1, bits: make([]uint64, size), image: emptyGrid(grid)}
	}
	// close(done)
	// close(gridch)
	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			close(cells[r][c].generations)
		}
	}
}
