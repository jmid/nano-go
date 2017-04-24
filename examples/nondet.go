package main

func main() { 
	ch := make(chan int);
	go func() { ch <-2 }()
	go func() { ch <-3 }()
	var x int;
	var y int;
	x = <- ch;
	y = <- ch;
	print(x);
	print(y)
}
