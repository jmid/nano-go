package main

func main() {
	ch := make(chan int);
	go func() {
		var x int;
		for {
			x = <- ch;
			print(x)
		}
	}()
	var x int; //int vars are initialized to 0
	for {
		ch <-x;
		x = x-1;
	}
}
