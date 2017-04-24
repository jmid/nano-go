package main

func main() {
	ch := make(chan int);
	go func() {
		ch <- 0;
		ch <- 1
	}()
	go func() { <-ch }()
	var y int;
	y = <-ch;
	print(y)
}
