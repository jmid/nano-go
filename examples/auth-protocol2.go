package main

/* simple auth protocol from Zafiropulo-al:TOC80, Fig.1 */
/* but with response data characterizing the response   */
func main() {
	acc_req := make(chan int);
	acc_response := make(chan int);
	acc_relinquished := make(chan int);
	go func() {         // server process
		var id int;
		for {
			id = <- acc_req;
			print(id);
			select {
			case acc_response <- 0: // deny
				//print("refused");
			case acc_response <- 1: // grant
				//print("granted");
				id = <- acc_relinquished;
			}
		}
	}()
	var response int;   // client process
	for {
		acc_req <- 0;
		response = <- acc_response;
		if response == 0 { } else { acc_relinquished <- 0 }
	}
}
