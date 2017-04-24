package main

/* simple auth protocol from Zafiropulo-al:TOC80, Fig.1 */
func main() {
	acc_req := make(chan int);
	acc_refused := make(chan int);
	acc_granted := make(chan int);
	acc_relinquished := make(chan int);
	go func() { // server process
		var id int;
		for {
			id = <- acc_req;
			select {
			case acc_refused <- id:
				//print("refused");
			case acc_granted <- id:
				//print("granted");
				id = <- acc_relinquished;
			}
		}
	}()
	var id int; // client process
	id = 0;
	for {
		acc_req <- id; // simple deadlock if omitted
		select {
		case id = <- acc_granted:
			acc_relinquished <- 0;
			//print("relinquished");
		case id = <- acc_refused:
		};
	}
}
