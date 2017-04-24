package main

func main() {
	a := make(chan int)
	b := make(chan int)
	// inlined foo
	go func () {
		b <- 42;
		<-a;
	}()
	a <- 42;
	<-b
}
