package main

func main() {
	ch1 := make(chan int)
	go func() {
		select {
		case ch1 <- 0:
		case ch1 <- 1:
		}
	}()
	go func() {
		select {  }
	}()
	var x int;
	x = <- ch1;
	print(x);
}
