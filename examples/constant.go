package main

func main() {
	ch0 := make(chan int);
	ch1 := make(chan int);
	go func () {
		var y int;
		ch0 <-0;
		for {
			y = <-ch1;
			ch0 <- (y-1);
//			print(y)
		}
	}()
	var x int;
	for {
		x = <-ch0;
		ch1 <- (x+1);
//		print(x)
	}
}
