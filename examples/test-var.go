package main

func main() {
	ch1 := make(chan int)
	go func() { ch1 <- 5 }()
	go func() { ch1 <- 2 }()
	var x int;
	print(x)
}
