package main

/* Highscore example from Midtgaard-Nielson-Nielson:SAS16 implemented with const */
func main() {
	ask := make(chan int);
	hsc := make(chan int);
	report := make(chan int);
	myscore := make(chan int);
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
	go func () {    // process for chosing arbitrary score (either 0 or max+2) via myscore channel
		var max int;
		max = 0;
		for {
			select {
			case myscore <- 0:
			case myscore <- max+2:
				max = max + 2
			}
		}
	}()
	var id int; 	// client process
	var best int;
	var new int;
	id = 0;
	for {
		ask <- id;
		best = <- hsc;
		new = <- myscore;
		if best < new {
			best = new;
			report <- best
		} else {}
	}
}
