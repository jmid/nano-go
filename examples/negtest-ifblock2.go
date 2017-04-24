package main

func main() {
	ch := make(chan int);
	go func() { }()
	var x int;
	if x==0 { ch <- 0; } else ch <- 1;
}
