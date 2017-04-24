package main

/* attempt to send message to yourself */
func main() {
	ch := make(chan int)
	go func() { }()
	var x int;
	ch <- 1;
	x = <- ch;
	print(x)
}
