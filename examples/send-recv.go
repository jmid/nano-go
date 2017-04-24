package main

func main() { 
	ch := make(chan int);
	go func() { ch <-42 }()
	var x int;
	x = <- ch;
	print(x)
}
