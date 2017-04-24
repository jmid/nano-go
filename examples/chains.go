package main

func main() {
	ch0 := make(chan int);
	ch1 := make(chan int);
	go func() {
		var x int;
		for {
			x = <- ch0;
			ch1 <- (x + 1);
//			print(x)
		}
	}()
	var y int;
	ch0 <-1;
	for {
		y = <- ch1;
		ch0 <- (y + 1);
//		print(y)
	}
}
