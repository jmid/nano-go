package main

/* example from Cousot-Cousot:ICALP80, p.125 */
func main() {
	ch0 := make(chan int)
	ch1 := make(chan int)
	go func() {
		var x int;
		x = 10;
		for 1<=x {
			ch0 <- x;   /* (was: P2) --- process names adapted to channel names */
			x = <- ch1;
			print(x);
		}
	}()
	var y int;
	y = <- ch0;    /* (was: P1) --- process names adapted to channel names */
	for !(y == 0) {
		ch1 <- y - 1;
		y = <- ch0;  /* deadlock on last iteration */
		print(y);
	}
}
