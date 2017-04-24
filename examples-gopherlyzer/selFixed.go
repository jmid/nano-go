package main

func main() {
	x := make(chan int) // changed bool to int
	y := make(chan int)
	//provide1
	go func() {
		x <- 1      // 1 ~ true
	}()
	//provide2
	go func() {
		y <- 0      // 0 ~ false
	}()
	var z int;          // explicit z declaration
	select {
        case z = <-x:
		print(z)
        case z = <-y:
		print(z)
	}
}
