package main


func main() {
	forks := make(chan int)

	// give1
	go func() { forks <- 1 }()
	// give2
	go func() { forks <- 1 }()
	// phil
	go func() {
		for {
			<-forks;
			<-forks;
			forks <- 1;
			forks <- 1;
		}
	}()

	for {
		<-forks;
		<-forks;
		forks <- 1;
		forks <- 1;
	}	
}
