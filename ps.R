"ps"<-
function(x, ps.intervals = NULL, lambda = 0, degree = 3, order = 3, ridge.adj
	 = 1e-005, ridge.inv = 0.0001)
{
# NOTE: see Instructions and Examples
# x: regressor of interest (also see signal.index and varying.index below) 
# ps.intervals: number of equally spaced B-spline intervals 
#	(the number of knots is equal to ps.int+2*degree+1
# lambda: non-negative regularization parameter for difference penalty
# degree: degree of B-spline basis
# order: order of difference penalty (0 is the ridge penalty)
# ridge.adj, ridge.inv: small positive numbers to stabilize 
#	linear dependencies among B-spline bases 
#
# Support functions: glass.wam(), gam.slist(), gam.wlist(), plot.glass()
#
# Reference: 
# Eilers, P.H.C. and Marx, B.D. (2002). Generalized Linear Additive Smooth Structures. 
#	Journal of Compuational and Graphical Statistics, 11(4): 758-783.
# Marx, B.D. and Eilers, P.H.C. (1998). Direct generalized linear modeling with 
#	penalized likelihood. CSDA, 28(2): 193-209.	
# Eilers, P.H.C. and Marx, B.D. (1996). Flexible smoothing with B-splines and penalties 
#	(with comments and rejoinder). Statistical Science, 11(2): 89-121.
#
#
# (c) 1998, 2003 Brian D. Marx
#
	number.knots <- ps.intervals + 2 * degree + 1
	x.index <- as.vector(x)
	smooth.dummy <- T
	xl <- min(x.index)
	xr <- max(x.index)
	xmax <- xr + 0.01 * (xr - xl)
	xmin <- xl - 0.01 * (xr - xl)
	dx <- (xmax - xmin)/ps.intervals
	nx <- names(x.index)
	nax <- is.na(x.index)
	if(nas <- any(nax))
		x.index <- x[!nax]
	sorder <- degree + 1
	if(!missing(ps.intervals)) {
		nAknots <- ps.intervals - 1
		if(nAknots < 1) {
			nAknots <- 1
			warning(paste("ps.intervals was too small; have used 2"
				))
		}
		if(nAknots > 0) {
			Aknots <- seq(from = xmin - degree * dx, to = xmax + 
				degree * dx, by = dx)
		}
		else knots <- NULL
	}
	basis <- spline.des(Aknots, x.index, sorder, 0 * x.index)$design
	sbasis <- NULL
	n.col <- ncol(basis)
	if(nas) {
		nmat <- matrix(NA, length(nax), n.col)
		nmat[!nax,  ] <- basis
		basis <- nmat
	}
	dimnames(basis) <- list(1:nrow(basis), 1:n.col)
	if((order - n.col + 1) > 0) {
		order <- n.col - 1
		warning(paste("order was too large; have used ", n.col - 1))
	}
	if(lambda < 0) {
		lambda <- 0
		warning(paste("lambda was negative; have used ", lambda))
	}
	if(lambda > 10000) {
		lambda <- 10000
		warning(paste("lambda was >10000; for stability have used", 
			lambda))
	}
	aug <- diag(n.col)
	if(order != 0) {
		for(tt in 1:order) {
			aug <- diff(aug)
		}
	}
	pen.aug <- sqrt(lambda) * aug
	attr(basis, "knots") <- Aknots
	if(ridge.adj == 0) {
		attr(basis, "pen.augment") <- pen.aug
	}
	if(ridge.adj != 0) {
		attr(basis, "pen.augment") <- rbind(pen.aug, sqrt(ridge.adj) * 
			diag(n.col))
	}
	attr(basis, "lambda") <- lambda
	attr(basis, "order") <- order
	attr(basis, "smooth.dummy") <- smooth.dummy	
	#	attr(basis, "ridge.gam") <- ridge.gam
	attr(basis, "ridge.inv") <- ridge.inv
	attr(basis, "ps.int") <- ps.intervals
	attr(basis, "degree") <- degree
	basis
}
"ps.wam"<-
function(x, y, w, s, which, smooth.frame, maxit = 20, tol = 1e-008, trace = F, 
	se = T, ...)
{
# first call to wam; set up some things
#on first entry, smooth.frame is a data frame with elements the terms to be
#smoothed in which
	if(is.data.frame(smooth.frame)) {
		first <- T
		data <- smooth.frame[, names(which), drop = F]
	}
	else first <- F
	if(first) {
		dx <- as.integer(dim(x))
		p <- dx[2]
		attr(data, "class") <- NULL
		pen <- lapply(data, attr, "pen.augment")
		ridge.inv <- unlist(lapply(data, attr, "ridge.inv"))[1]	
	#ridge.gam <- unlist(lapply(data, attr, "ridge.gam"))[1]
		ps.int <- lapply(data, attr, "ps.int")
		ps.degree <- lapply(data, attr, "degree")
		smooth.dummy <- lapply(data, attr, "smooth.dummy")
		smooth.index <- which[unlist(smooth.dummy)]
		pendim <- sapply(pen, dim)
		n <- dx[1]
		nrow.p <- sum(pendim[1,  ])
		p.augm <- matrix(0, nrow.p, p)
		num.ps <- length(which)
		startt <- 0
		for(i in 1:num.ps) {
			index <- seq(pendim[1, i]) + startt
			p.augm[unlist(index), unlist(which[i])] <- pen[[i]]
			startt <- max(index)
		}
		zeros <- rep(0, nrow.p)	
	#if(length(which) > 1 & ridge.gam > 0) {
#	p.augm <- rbind(p.augm, sqrt(ridge.gam) * diag(p))
#	zeros <- rep(0, nrow.p + p)
#}
		lin.dim <- ncol(as.matrix(x[,  - c(unlist(which))]))
		smooth.frame <- list(lin.dim = lin.dim, pendim = pendim, num.ps
			 = num.ps, p = p, n = n, p.augm = p.augm, zeros = zeros,
			smooth.index = smooth.index, ridge.inv = ridge.inv, 
			ps.int = ps.int, ps.degree = ps.degree, gsmooth.dummy
			 = unlist(smooth.dummy))
		first <- F
	}
	storage.mode(tol) <- "double"
	storage.mode(maxit) <- "integer"
	storage.mode(y) <- "double"
	storage.mode(w) <- "double"
	gsmooth.dummy <- smooth.frame$gsmooth.dummy
	ridge.inv <- smooth.frame$ridge.inv
	lin.dim <- smooth.frame$lin.dim
	num.ps <- smooth.frame$num.ps
	ps.int <- smooth.frame$ps.int
	ps.degree <- smooth.frame$ps.degree
	smooth.index <- smooth.frame$smooth.index
	p <- smooth.frame$p
	pendim <- smooth.frame$pendim
	p.augm <- smooth.frame$p.augm
	zeros <- smooth.frame$zeros
	n <- smooth.frame$n
	yaug <- as.vector(c(y, zeros))
	waug <- as.vector(c(w, (zeros + 1)))
	xaug <- as.matrix(rbind(x, p.augm))
	nl.dim <- rep(0, num.ps)
	startt <- 0
	for(i in 1:num.ps) {
		index <- seq(pendim[1, i]) + startt
		xaugi <- xaug[, unlist(which[i])]
		xi <- x[, unlist(which[i])]
		nl.dim[i] <- sum(diag(solve(t(xaugi) %*% (waug * xaugi)) %*% (t(
			xi) %*% (waug[1:n] * xi))))
		startt <- max(index)
	}
	fit <- lsfit(xaug, yaug, wt = waug, intercept = F)
	names(fit$df) <- dimnames(ps)[[2]]
	names(fit$coef) <- labels(x)[[2]]
	Rridge <- ridge.inv * diag(p)
	Bbread <- t(xaug) %*% (waug * xaug)
	Ccheese <- t(x) %*% (w * x)
	iBbread <- solve(Bbread + Rridge)
	iCcheese <- solve(Ccheese + Rridge)
	SW <- (Bbread %*% iCcheese %*% Bbread)	# + Rridge
	iSW <- iBbread %*% Ccheese %*% iBbread
	SW1 <- SW
	SW1[lower.tri(SW1)] <- 0
	SW2 <- t(SW1)
	Ddiag <- diag(diag(SW))
	SW <- SW1 + SW2 - Ddiag
	CH <- chol(SW, pivot = T)
	PP <- order(attr(CH, "pivot"))
	Half <- CH[, PP]
	Rqr <- qr(Half)
	R <- qr.R(Rqr)
	dimnames(R)[[1]] <- dimnames(R)[[2]] <- c(Rqr$pivot)[1:Rqr$rank]
	hatt <- hat(sqrt(waug) * xaug, intercept = F)[1:n]
	rl <- list(coefficients = fit$coef, residuals = fit$residuals[1:n], 
		lin.dim = lin.dim, fitted.values = y - fit$residuals[1:n], rank
		 = Rqr$rank, pivot = Rqr$pivot, R = R, qraux = Rqr$qraux, iSW
		 = iSW, hat = hatt, df.residual = n - sum(nl.dim) - lin.dim, 
		nl.dim = nl.dim, gsmooth.index = smooth.index, ridge.inv = 
		ridge.inv, ps.int = ps.int, ps.degree = ps.degree, 
		gsmooth.dummy = gsmooth.dummy)
	c(list(smooth.frame = smooth.frame), rl)
}
"plot.ps"<-
function(ps.object, type = "smooth", index = 1, t.domain, se.bands = T, scale
	 = T, rug.on = T, Resol = 80)
{
# ps.object: object from ps.object <- gam(y~ps(.)+...)
# type: "smooth"
# index: component number of the specified type (e.g. 1 if first smooth term, 2 if second)
# t.domain: variable that B-spline basis is computed from 
#	(eg: regressor for smooth, time for varying, freq for signal)
# scale: T (unbiased est of scale param RSS/df) or F (set to 1)
# rug: T for t.domain ticks on x-axis, F otherwise
# Resol: resolution of plot (number of equally space points to generate smooth)
	obj <- ps.object
	if(type == "smooth") {
		iindex <- as.vector(unlist(obj$gsmooth.index[index]))
		ps.intervals <- unlist(obj$ps.int[(obj$gsmooth.dummy) == T])
		degree <- unlist(obj$ps.degree[(obj$gsmooth.dummy) == T])
	}
	if(length(ps.intervals) < index) {
		warning(paste("Error: index argument too large"))
	}
	ps.intervals <- ps.intervals[index]
	degree <- degree[index]
	mmin <- min(iindex)
	mmax <- max(iindex)
	tt.domain <- seq(min(t.domain), max(t.domain), length = Resol)
	psm. <- glass(tt.domain, ps.intervals, degree = degree)
	ccoef <- obj$coef[mmin:mmax]
	ssmooth <- psm. %*% ccoef
	if(type == "smooth") {
		ssmooth <- ssmooth - mean(ssmooth)
	}
	if(!se.bands) {
		plot(tt.domain, ssmooth, type = "l", xlab = " ", ylab = " ")
	}
	ssigma <- NULL
	if(se.bands) {
		ssigma <- 1
		R <- obj$R
		if(scale) {
#			ssigma <- sqrt(obj$dev/obj$df.r)
#			ssigma <- summary.lm(obj)$sigma
			ssigma <- sqrt(sum((obj$resid * sqrt(obj$weights))^2)/
				obj$df.r)
		}
		icwhole <- obj$iSW
		icwhole. <- icwhole[mmin:mmax, mmin:mmax]
		var.p <- psm. %*% icwhole. %*% t(psm.)
		se <- ssigma * (sqrt(diag(var.p)))
		upper <- ssmooth + 2 * se
		lower <- ssmooth - 2 * se
		ssmooth <- cbind(lower, ssmooth, upper)
		matplot(tt.domain, ssmooth, type = "l", lty = c(3, 1, 3), xlab
			 = " ", ylab = " ")
	}
	if(rug.on) {
		rug(t.domain)
	}
	list(smooth.grid = ssmooth, scale = ssigma, degree = degree)
}
"gam.slist"<-
c("s", "lo", "ps", "glass")
"gam.wlist"<-
c("s", "lo", "ps", "glass")
