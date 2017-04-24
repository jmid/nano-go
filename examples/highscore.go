package main

/* Highscore example from Midtgaard-Nielson-Nielson:SAS16 implemented with const */
func main() {
	ask := make(chan int);
	hsc := make(chan int);
	report := make(chan int);
	go func() {  	// server process
		var highscore int;
		var new int;
		var cid int;
		highscore = 0;
		for {
			select {
			case cid = <- ask:
				hsc <- highscore;
				print(cid);
				print(highscore)
			case new = <- report:
				if highscore < new { highscore = new } else {}
			}
		}
	}()
	// client process
	var id int;
	var best int;
	var new int;
	id = 0;
	for {
		ask <- id;
		best = <- hsc;
		new = 10;
		if best < new {
			best = new;
			report <- best
		} else {}
	}
}
