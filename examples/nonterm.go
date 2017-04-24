package main

func main() {
	ch := make(chan int);
	go func() {
		var y int;
		y = 1000;
		for 0 < y {
			ch <- y;
			y = y-1;
		};
		print(y)
	}()
	var x int;
	x = 1;
	for 0 < x { x = <- ch }
}
