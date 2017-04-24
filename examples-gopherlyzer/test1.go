package main

func main() {
	x := make(chan int)
	y := make(chan int)
	z := make(chan int)

	// inlined A
	go func() {
		x <- 4;
		<-z;
		x <- 5;
	}()

	// inlined B
	go func() {
		<-x;
		<-y;
	}()

	// inlined C
	<-x;
	// inlined C1
	z <- 4;
	y <- 3;
}
