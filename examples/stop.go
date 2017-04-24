package main

func main() { 
	ch := make(chan int);
	go func() {
		ch <- 1;
		select {}
	}()
	var x int;
	x = <- ch;
	print(x);
	select {}
}
