package main

func main() {
	ch1 := make(chan int);
	ch2 := make(chan int);
	go func() { 
		ch1 <-1;
		ch2 <-2; }()
	go func() {
		var x int;
		x = <-ch1;
		ch2 <- x+1; }()
	var y int;
	y = <- ch2;
	print(y)
}
