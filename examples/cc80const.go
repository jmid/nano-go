package main

/* example from Cousot-Cousot:ICALP80, conclusion p.132 */
func main() {
	ch := make(chan int)
	go func() {
		ch <- 1;     /* (was: P2) --- process names adapted to channel names */
		ch <- 2;
	}()
	var x int;
	var y int;
	x = 1;
	y = 2;
	x = <- ch;    /* (was: P1) --- process names adapted to channel names */
	y = <- ch;
	print(x);
	print(y);
}
