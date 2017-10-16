# Author: Julian Zhou @julianqz

# Function to plot barplot of means and standard error bars
# Based on barplot() and accepts arguments for barplot() via '...'
# Input:
# - means: a vector of means to be plotted; passed to barplot () for 'heights'
# - ses: a vector of standard errors to be plotted as error bars
# - widths: passed to barplot() for 'width'; see ?barplot for details
# - spaces: passed to barplot() for 'space'; see ?barplot for details
# - horizontal: passed to barplot() for 'horiz'; if TRUE, layout will be horizontal
# - ymax.excess: the fraction to multiply ymax by for adjusting ylim; must be >=1
# - err.bar.col: color of the error bars
# - err.bar.lty: line type of the error bars
# - err.bar.lwd: line thickness of the error bars
# - err.bar.pch: shape of the dot for the means
# - return.bar.x: whether to return x-coordinates of the bars; default is FALSE
# - ...: additional arguments to be passed to barplot(); see ?barplot for details
# Output:
# - A barplot of means with standard error bars indicated around the means
# - If return.bar.x=TRUE, coordinates of the error bars (i.e. centers of the mean bars)
# Note:
# - return.bar.x=TRUE might be helpful for customizing x-axis labels (see example below)
# - 'height', 'width', 'space', and 'horiz', 'ylim', 'xlim' should not be passed as 
#   part of '...' to barplot()
# Examples:
# - See the 'Testing' section below
errbarplot = function(means, ses, widths=1, spaces=0.2, horizontal=FALSE, ymax.excess=1.02,
                      err.bar.col=1, err.bar.lty=1, err.bar.lwd=1, err.bar.pch=16,
                      return.bar.x=FALSE,
                      ...) {
    ### check precondition
    n = length(means)
    stopifnot(n==length(ses))
    stopifnot( (length(widths)==n) | (length(widths)==1) )
    stopifnot( (length(spaces)==n) | (length(spaces)==1) )
    stopifnot(ymax.excess>=1)
    
    ### ylim after taking into account of SEs
    ymax = max(means+ses, na.rm=T)
    ymin = min(means-ses, na.rm=T)
    
    ### x-coords of error bars
    if (length(widths)==1) {widths=rep(widths, n)}
    if (length(spaces)==1) {spaces=rep(spaces, n)}
    # space, as defined in ?barplot, is a fraction based off width
    # convert fraction to absolute amount of space
    spaces.abs = widths * spaces
    
    se.x = sapply(1:n, function(i){
        if (i==1) {
            # special treatment: sum(widths[1:(i-1)]) won't work properly for i=1
            widths[i]/2 + spaces.abs[i]
        } else {
            # previous bar widths + half current bar width + previous inter-bar spaces 
            sum(widths[1:(i-1)]) + widths[i]/2 + sum(spaces.abs[1:i])
        }
    })
    
    ### base barplot
    # per ?barplot, single-number input of width will have no effect unless xlim is set
    if (horizontal) {
        ## horizontal bars
        barplot(height=means, width=widths, space=spaces, horiz=horizontal,
                # 0.3 instead of 0 in order for lwd effect of abline at zero to show  
                xlim=c(min(ymax.excess*ymin, -0.3), max(0.3, ymax.excess*ymax)), #*
                ylim=c(0, 1.02*(sum(widths)+sum(spaces.abs))), 
                ...)
        abline(v=0, lwd=2)
    } else {
        ## vertical bars
        barplot(height=means, width=widths, space=spaces, horiz=horizontal,
                # 0.3 instead of 0 in order for lwd effect of abline at zero to show  
                ylim=c(min(ymax.excess*ymin, -0.3), max(0.3, ymax.excess*ymax)), #*
                xlim=c(0, 1.02*(sum(widths)+sum(spaces.abs))), 
                ...)
        abline(h=0, lwd=2)
    }
    
    
    ### error bars
    err.ub = means+ses
    err.lb = means-ses
    # if all means>0 and any lower bound<0, set that LB to 0
    #if (all(means>0, na.rm=T)) {err.lb = ifelse(err.lb<0, 0, err.lb)}
    # if all means<0 and any upper bound>0, set that UB to 0
    #if (all(means<0, na.rm=T)) {err.ub = ifelse(err.ub>0, 0, err.ub)}
    
    for (i in 1:n) {
        # width of error bar
        err.width = widths[i]*0.1 #*
        
        if (horizontal) {
            ## horizontal bars
            # upper bound (won't show if UB is NA)
            segments(y0=se.x[i]-err.width, y1=se.x[i]+err.width, 
                     x0=err.ub[i], x1=err.ub[i], 
                     col=err.bar.col, lty=err.bar.lty, lwd=err.bar.lwd)
            # lower bound (won't show if LB is NA)
            segments(y0=se.x[i]-err.width, y1=se.x[i]+err.width, 
                     x0=err.lb[i], x1=err.lb[i],
                     col=err.bar.col, lty=err.bar.lty, lwd=err.bar.lwd)
            # vertical line connecting UB and LB (won't show if either UB or LB is NA)
            segments(y0=se.x[i], y1=se.x[i], x0=err.lb[i], x1=err.ub[i],
                     col=err.bar.col, lty=err.bar.lty, lwd=err.bar.lwd)
            # dot for mean (won't show if mean is NA)
            points(means[i], se.x[i], col=err.bar.col, lwd=err.bar.lwd, pch=err.bar.pch)
        } else {
            ## vertical bars
            # upper bound (won't show if UB is NA)
            segments(x0=se.x[i]-err.width, x1=se.x[i]+err.width, 
                     y0=err.ub[i], y1=err.ub[i], 
                     col=err.bar.col, lty=err.bar.lty, lwd=err.bar.lwd)
            # lower bound (won't show if LB is NA)
            segments(x0=se.x[i]-err.width, x1=se.x[i]+err.width, 
                     y0=err.lb[i], y1=err.lb[i],
                     col=err.bar.col, lty=err.bar.lty, lwd=err.bar.lwd)
            # vertical line connecting UB and LB (won't show if either UB or LB is NA)
            segments(x0=se.x[i], x1=se.x[i], y0=err.lb[i], y1=err.ub[i],
                     col=err.bar.col, lty=err.bar.lty, lwd=err.bar.lwd)
            # dot for mean (won't show if mean is NA)
            points(se.x[i], means[i], col=err.bar.col, lwd=err.bar.lwd, pch=err.bar.pch)
        }
    }
    
    if (return.bar.x) {return(se.x)}
    
}

# Testing
run.test = FALSE

if (run.test) {
    # Generate means and SEs
    set.seed(79582)
    means = runif(n=5, min=30, max=100)
    names(means) = LETTERS[1:length(means)]
    ses = rexp(n=5, rate=1/6)
    
    # Basic example
    # Positive means
    # Notice that border, col, xlab, and ylab are passed to barplot()
    # Vertical bars
    errbarplot(means, ses, widths=2, spaces=0.5, horizontal=FALSE,
               err.bar.col="hotpink", err.bar.lty=3, err.bar.lwd=2, err.bar.pch=18, 
               border=NA, col="skyblue", xlab="Group", ylab="Mean")
    # Horizontal bars
    errbarplot(means, ses, widths=2, spaces=0.5, horizontal=TRUE,
               err.bar.col="hotpink", err.bar.lty=3, err.bar.lwd=2, err.bar.pch=18, 
               border=NA, col="skyblue", xlab="Group", ylab="Mean")
    
    # Negative means
    errbarplot(-means, ses, horizontal=FALSE)
    errbarplot(-means, ses, horizontal=TRUE)
    
    # A mixture of positive and negative means
    errbarplot(c(means[1:2], -means[3], means[4:5]), ses, horizontal=FALSE)
    errbarplot(c(means[1:2], -means[3], means[4:5]), ses, horizontal=TRUE)
    
    # Large SE
    errbarplot(means, c(100, ses[2:5]), horizontal=FALSE)
    errbarplot(means, c(100, ses[2:5]), horizontal=TRUE)
    
    errbarplot(-means, c(100, ses[2:5]), horizontal=FALSE)
    errbarplot(-means, c(100, ses[2:5]), horizontal=TRUE)
    
    errbarplot(c(means[1:2], -means[3], means[4:5]), c(100, ses[2:5]), horizontal=FALSE)
    errbarplot(c(means[1:2], -means[3], means[4:5]), c(100, ses[2:5]), horizontal=TRUE)
    
    # NAs allowed
    # One of the means and its corresponding SE are NA
    errbarplot(c(means[1:2], NA, means[3:5]), c(ses[1:2], NA, ses[3:5]), horizontal=FALSE)
    errbarplot(c(means[1:2], NA, means[3:5]), c(ses[1:2], NA, ses[3:5]), horizontal=TRUE)
    # One of the SE is NA (for whatever reason)
    errbarplot(c(means[1:2], 35, means[3:5]), c(ses[1:2], NA, ses[3:5]), horizontal=FALSE)
    errbarplot(c(means[1:2], 35, means[3:5]), c(ses[1:2], NA, ses[3:5]), horizontal=TRUE)
    # One of the mean is NA even though SE isn't (unlikely scenario; for demonstration only)
    errbarplot(c(means[1:2], NA, means[3:5]), c(ses[1:2], 2, ses[3:5]), horizontal=FALSE)
    errbarplot(c(means[1:2], NA, means[3:5]), c(ses[1:2], 2, ses[3:5]), horizontal=TRUE)
    
    # Using return.bar.x to help customize x-axis labels
    par(mar=c(7,4,2,2))
    bar.x = errbarplot(means, ses, horizontal=F, border=NA, return.bar.x=T, ylab="Mean", names.arg="")
    axis(side=1, at=bar.x, labels=paste("Group", LETTERS[1:5]), tick=F, las=2)
    mtext(text="Focus Groups", side=1, line=5)
    
    par(mar=c(5,7,2,2))
    bar.x = errbarplot(means, ses, horizontal=T, border=NA, return.bar.x=T, xlab="Mean", names.arg="")
    axis(side=2, at=bar.x, labels=paste("Group", LETTERS[1:5]), tick=F, las=2)
    mtext(text="Focus Groups", side=2, line=5)
}
