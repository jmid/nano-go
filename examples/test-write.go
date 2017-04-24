package main

func main() {
	ch1 := make(chan int)
	go func() { ch1 <- 1 }()
	go func() { ch1 <- 2 }()
	if true {} else {};
}
