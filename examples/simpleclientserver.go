package main

/* simple client server with client performing only one ask-reply */
func main() {
	init := make(chan int)
	status := make(chan int)
	ask := make(chan int)
	reply := make(chan int)
	done := make(chan int)
	go func() {
		var y int;
		var cardset int;
		var cap int;
		var ok int; var notok int; ok = 1; notok = 0;
		cardset = 0;
		cap = 1;  // some capacity (1)
		for {
			select {
			case y = <- init:
				if cardset < cap { /* model 'set' by a cardinality  abstraction */
					cardset = cardset + 1;
					status <- ok
				} else {
					status <- notok
				}
			case y = <- ask:
				reply <- y             /* in or not in set? */
			case y = <- done:
				cardset = cardset - 1; /* in or not in set? */
			}
		}
	}()
	var y int;   // client process
	var res int;
	var ai int;
	var ok int; ok = 1;
	ai = 0; // client id = 0
	for {
		init <- ai;
		y = <- status;
		print(y);
		for !(y == ok) {  // repeat until y == ok
			init <- ai;
			y = <- status;
			print(y);
		};
		ask <- ai;
		res = <- reply;
		done <- ai;
		print(res);
	}
}
