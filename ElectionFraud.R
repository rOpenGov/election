
countryname = data.frame(Votes=c(23,24,25), NValid=c(50,40,45),NVoters=c(150,150,150))
FraudFits <- function(countryname, thres, f1range, f2range, arange,iterations) {
	bins <- seq(0, 1, 0.01)
	N = length(countryname$Votes)
	Attendance = countryname$NValid/countryname$NVoters

	v = mat.or.vec(N,2) # initialize matrix for voting 

	v[,1] = countryname$Votes/countryname$NValid
	v[,2] = (countryname$NValid-countryname$Votes)/countryname$NValid

	#detect first local maximum (code applied from )
	w_temp = hist(v[,1], seq(min(v[,1]),max(v[,1]), length.out=length(bins+1), plot=FALSE)
	n = w_temp$mids; x = w$counts
	s1 = diff(n[10:end])

	v_max = x[which(n==max(n)),1]

	lambda_fraud = c(head(x[which(s1 < -thres)],1)+1, 1-head(x[which(s1 < -thres)],1)+9])
	w_temp = hist(Attendance[!is.na(Attendance)], bins)
	n = w_temp$mids; x = w$counts
	s1 = diff(n[15:length(n)])

	p_Att <- head(x[which(s1 < -thres)],1)+14

	s1 = c(); s2 = c(); s3 = c()

	# problems with following code snippet, debug it later
	for (i in 1:length(Attendance)) {
		if(v[i,1] < lambda_fraud[i]) {
			if(Attendance[i] < p_Att) {
				s1 = c(s1, Attendance[i] - p_Att)
			} 
			s2 = c(s2, v[i,1]-lambda_fraud[i])
		}

		if( v[i,1] > v_max) {
			s3 = c(s3, v[i,1]-v_max)
		}
	}

	stdAtt <- sqrt(sum(s1^2)/length(s1))
	theta <- sqrt(sqrt(sum(s3^2)/length(s3)))
	sigma <- c(sqrt(2*sum(s2^2)/length(s2)), sqrt(2*sum(s2^2)/length(s2)))

	n0 = hist(v[,1], seq(min(v[,1]),max(v[,1])), length.out=length(bins+1), plot=FALSE)$counts
	x0 = hist(v[,1], seq(min(v[,1]),max(v[,1])), length.out=length(bins+1), plot=FALSE)$mids

	FraudFits = c()
  iterations <- 7
	for (i in 1:iterations) {
		Ffits <- rep(4,length(f1range)*length(f2range)*length(arange))
		wr <- 1

		

	}

