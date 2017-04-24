package main

// basic deadlock example (from Hugo)
func main() {
	ch_a := make(chan int)
	ch_b := make(chan int)
	go func() {
		var x int;
		x = <- ch_a;
		ch_b <- 1;
		print(x)
	}()
	var y int;
	y = <- ch_b;
	ch_a <- 2;
	print(y)
}
